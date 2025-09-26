use crate::lang::RuleId;
use crate::tree::{Tree, TreeAlloc, TreeId};
use crate::utils::first_valid;
use crate::{
    lang::{Grammar, LexerPattern},
    tree::HoleType,
};
use regex::Regex;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

pub struct LexIter<'a> {
    alloc: TreeAlloc,
    text: &'a str,
    pos: usize,
    matchers: Vec<(RuleId, LexMatcher)>,
    // previous token/hole produced by the lexer (for linking)
    prev: Option<Arc<Tree>>,
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

    fn link_nodes(&mut self, node: Arc<Tree>) {
        // Link the current node to the previous node using OnceLock::set
        if let Some(prev_node) = &self.prev {
            match Arc::as_ref(prev_node) {
                Tree::Token { right, .. } | Tree::Hole { right, .. } => {
                    // set may fail if already set; ignore the error
                    let _ = right.set(node.clone());
                }
                _ => {}
            }
        }
        self.prev = Some(node.clone());
    }

    fn create_token(&mut self, id: RuleId, text: &str) -> Arc<Tree> {
        self.alloc.new_token(
            id,
            text.to_string(),
            None,
            self.prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn create_hole(&mut self, ch: char) -> Arc<Tree> {
        self.alloc.new_hole(
            ch.to_string(),
            HoleType::Incomplete,
            None,
            self.prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn process_match(&mut self, l: usize, id: RuleId) -> Arc<Tree> {
        let text = &self.text[self.pos..self.pos + l];
        self.pos += l;
        let node = self.create_token(id, text);
        self.link_nodes(node.clone());
        node
    }

    fn process_hole(&mut self, ch: char) -> Arc<Tree> {
        self.pos += ch.len_utf8();
        let node = self.create_hole(ch);
        self.link_nodes(node.clone());
        node
    }

    fn next_token_or_hole(&mut self) -> Option<Arc<Tree>> {
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
    ) -> Option<Arc<Tree>> {
        let res = first_valid(matcher_refs.into_iter().map(move |(id, mref)| {
            let rr = raw_rem; // copy
            move |_flag: &AtomicBool| -> Result<(usize, RuleId), ()> {
                let s = rr.as_str();
                let lm = unsafe { mref.get() }.match_len(s);
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
    fn advance(&mut self) -> Option<Arc<Tree>> {
        if self.pos >= self.text.len() {
            return None;
        }

        if self.matchers.len() <= 4 {
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
    type Item = Arc<Tree>;
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
        use crate::pretty::Pretty;

        let alloc = TreeAlloc::new();
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        let mut pretty = Pretty::new(&alloc, &grammar);
        println!("EBNF Grammar:\n{}", grammar);
        let text = "1+1*233*****4";
        let mut lexer = LexIter::new(alloc.clone(), &grammar, text);
        let tokens: Vec<_> = lexer.by_ref().collect();
        for t in &tokens {
            print!("{}", pretty.pretty_tree(t));
            t.left().map(|l| print!("\t\tL: {}", pretty.pretty_tree(l)));
            t.right()
                .map(|r| println!("\t\tR: {}", pretty.pretty_tree(r)));
        }
    }
}

pub struct PartialParser<'a> {
    alloc: TreeAlloc,
    grammar: &'a Grammar,
    path: Vec<TreeId>,
}
