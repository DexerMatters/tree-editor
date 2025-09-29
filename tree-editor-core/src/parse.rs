use crate::lang::RuleId;
use crate::tree::{HasSiblings, Tree, TreeAlloc, TreeRef};
use crate::utils::first_valid;
use crate::{
    lang::{Grammar, LexerPattern},
    tree::HoleType,
};
use regex::Regex;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct LexIter<'a> {
    alloc: TreeAlloc,
    text: &'a str,
    pos: usize,
    matchers: Vec<(RuleId, LexMatcher)>,
    // previous token/hole produced by the lexer (for linking)
    prev: Option<TreeRef>,
}

enum LexMatcher {
    Terminal { ptr: *const u8, len: usize },
    Regex(Regex),
}
unsafe impl Send for LexMatcher {}
unsafe impl Sync for LexMatcher {}

impl LexMatcher {
    #[inline]
    unsafe fn terminal_str(&self) -> &str {
        match self {
            LexMatcher::Terminal { ptr, len } => unsafe {
                let bytes = std::slice::from_raw_parts(*ptr, *len);
                std::str::from_utf8_unchecked(bytes)
            },
            _ => unreachable!(),
        }
    }
    #[inline]
    fn match_len(&self, s: &str) -> Option<usize> {
        match self {
            LexMatcher::Terminal { .. } => unsafe {
                let t = self.terminal_str();
                s.starts_with(t).then(|| t.len())
            },
            LexMatcher::Regex(re) => re.find(s).map(|m| m.end()),
        }
    }

    /// Variant of match_len that cooperatively checks a cancellation flag before
    /// performing potentially long-running work (regex matching).
    #[inline]
    fn match_len_with_done(&self, s: &str, done: &AtomicBool) -> Option<usize> {
        // Quick cooperative cancellation check before doing work.
        if done.load(Ordering::Acquire) {
            return None;
        }
        match self {
            LexMatcher::Terminal { .. } => unsafe {
                let t = self.terminal_str();
                // terminal match is cheap; no further checks necessary
                s.starts_with(t).then(|| t.len())
            },
            LexMatcher::Regex(re) => {
                // Check the flag again just before invoking regex to avoid starting
                // expensive work when another worker already succeeded.
                if done.load(Ordering::Acquire) {
                    return None;
                }
                re.find(s).map(|m| m.end())
            }
        }
    }
}

impl<'a> LexIter<'a> {
    pub fn new(alloc: TreeAlloc, grammar: &'a Grammar, text: &'a str) -> Self {
        let mut matchers = Vec::with_capacity(grammar.lexer_rules.len());
        for (id, rule) in &grammar.lexer_rules {
            let m = match &rule.pattern {
                LexerPattern::Terminal(s) => LexMatcher::Terminal {
                    ptr: s.as_ptr(),
                    len: s.len(),
                },
                LexerPattern::Regex(r) => {
                    let re = Regex::new(&format!("^(?:{})", r)).expect("bad regex");
                    LexMatcher::Regex(re)
                }
            };
            matchers.push((*id, m));
        }
        Self {
            alloc,
            text,
            pos: 0,
            matchers,
            prev: None,
        }
    }

    /// Collect tokens using the single-threaded path (no parallel matcher threads).
    pub fn collect_single(&mut self) -> Vec<TreeRef> {
        let mut v = Vec::new();
        while let Some(node) = self.advance() {
            v.push(node);
        }
        v
    }

    fn link_nodes(&mut self, node: TreeRef) {
        // Link the current node to the previous node
        if let Some(prev_node) = &self.prev {
            // use trait-based sibling access instead of pattern-matching on concrete types
            if let Some(sib) = prev_node.as_siblings() {
                // only set right if it is not already set
                if sib.right().is_none() {
                    sib.set_right(node.clone());
                }
            }
        }
        self.prev = Some(node.clone());
    }

    fn create_token(&mut self, id: RuleId, text: &str) -> TreeRef {
        self.alloc.new_token(
            id,
            text.to_string(),
            None,
            self.prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn create_hole(&mut self, ch: char) -> TreeRef {
        self.alloc.new_hole(
            ch.to_string(),
            HoleType::UndefinedToken,
            None,
            self.prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn process_match(&mut self, l: usize, id: RuleId) -> TreeRef {
        let text = &self.text[self.pos..self.pos + l];
        self.pos += l;
        let node = self.create_token(id, text);
        self.link_nodes(node.clone());
        node
    }

    fn process_hole(&mut self, ch: char) -> TreeRef {
        self.pos += ch.len_utf8();
        let node = self.create_hole(ch);
        self.link_nodes(node.clone());
        node
    }

    fn next_token_or_hole(&mut self) -> Option<TreeRef> {
        let remaining = &self.text[self.pos..];
        let mut best: Option<(usize, RuleId)> = None;

        for (id, m) in &self.matchers {
            if let Some(l) = m.match_len(remaining) {
                if l > 0 && best.map(|(bl, _)| l > bl).unwrap_or(true) {
                    best = Some((l, *id));
                }
            }
        }

        if let Some((l, id)) = best {
            return Some(self.process_match(l, id));
        }

        let ch = remaining.chars().next().unwrap();
        Some(self.process_hole(ch))
    }

    fn next_parallel(
        &mut self,
        raw_rem: RawSlice,
        matcher_refs: Vec<(RuleId, MatcherRef)>,
    ) -> Option<TreeRef> {
        let res = first_valid(matcher_refs.into_iter().map(move |(id, mref)| {
            let rr = raw_rem; // copy
            move |flag: &AtomicBool| -> Result<(usize, RuleId), ()> {
                // early return if someone else already found a match
                if flag.load(Ordering::Acquire) {
                    return Err(());
                }
                let s = rr.as_str();
                // use the cancellation-aware matcher variant
                let lm = unsafe { mref.get() }.match_len_with_done(s, flag);
                if let Some(l) = lm {
                    if l > 0 {
                        return Ok((l, id));
                    }
                }
                Err(())
            }
        }));

        if let Ok((l, id)) = res {
            return Some(self.process_match(l, id));
        }

        let ch = raw_rem.as_str().chars().next().unwrap();
        Some(self.process_hole(ch))
    }

    // private advance used by Iterator impl
    fn advance(&mut self) -> Option<TreeRef> {
        if self.pos >= self.text.len() {
            return None;
        }
        let mlen = self.matchers.len();
        if mlen <= 4 {
            // single-threaded path
            self.next_token_or_hole()
        } else {
            let raw_rem = RawSlice {
                ptr: self.text[self.pos..].as_ptr(),
                len: self.text[self.pos..].len(),
            };
            let matcher_refs: Vec<(RuleId, MatcherRef)> = self
                .matchers
                .iter()
                .map(|(id, m)| (*id, MatcherRef(m as *const _)))
                .collect();
            self.next_parallel(raw_rem, matcher_refs)
        }
    }
}

// implement Iterator for LexIter
impl<'a> Iterator for LexIter<'a> {
    type Item = TreeRef;
    fn next(&mut self) -> Option<Self::Item> {
        self.advance()
    }
}

// Raw slice wrapper to send remaining text pointer to threads without cloning.
#[derive(Copy, Clone)]
struct RawSlice {
    ptr: *const u8,
    len: usize,
}
unsafe impl Send for RawSlice {}
unsafe impl Sync for RawSlice {}
impl RawSlice {
    fn as_str(&self) -> &str {
        unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.ptr, self.len)) }
    }
}

// Wrapper for matcher raw pointer
#[derive(Copy, Clone)]
struct MatcherRef(*const LexMatcher);
unsafe impl Send for MatcherRef {}
unsafe impl Sync for MatcherRef {}
impl MatcherRef {
    unsafe fn get(&self) -> &LexMatcher {
        unsafe { &*self.0 }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[cfg(feature = "macros")]
    use crate::ebnf_grammar;

    #[test]
    #[cfg(feature = "macros")]
    fn test_lexer() {
        use crate::render::Render;

        let alloc = TreeAlloc::new();
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        let mut render = Render::new(&alloc, &grammar);
        println!("EBNF Grammar:\n{}", grammar);
        let text = "1+1*233*****4";
        let mut lexer = LexIter::new(alloc.clone(), &grammar, text);
        let tokens: Vec<_> = lexer.by_ref().collect();
        for t in &tokens {
            print!("{}", render.pretty_tree(t));
            // use as_siblings() for left/right access
            if let Some(sib) = t.as_siblings() {
                if let Some(l) = sib.left() {
                    print!("\t\tL: {}", render.pretty_tree(l));
                }
                if let Some(r) = sib.right() {
                    println!("\t\tR: {}", render.pretty_tree(r));
                }
            }
        }
    }
}
//     grammar: &'a Grammar,
//     path: Vec<TreeId>,
// }
