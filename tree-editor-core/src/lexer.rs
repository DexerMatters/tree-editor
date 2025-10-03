use crate::lang::RuleId;
use crate::lang::{Grammar, LexerPattern};
use crate::tree::{TokenRef, TreeAlloc};
use crate::utils::first_valid;
use regex::Regex;
use std::sync::atomic::{AtomicBool, Ordering};

#[derive(Debug, Clone)]
pub struct Lexer {
    alloc: TreeAlloc,
    matchers: Vec<(RuleId, LexMatcher)>,
}

#[derive(Debug, Clone)]
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

impl Lexer {
    pub fn new(alloc: TreeAlloc, grammar: &Grammar) -> Self {
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
        Self { alloc, matchers }
    }

    pub fn tokenize(&self, text: &str) -> Vec<TokenRef> {
        let mut pos = 0;
        let mut prev = None;
        let mut result = Vec::new();
        while pos < text.len() {
            let node = self.advance(text, &mut pos, &mut prev);
            result.push(node);
        }
        result
    }

    fn link_nodes(prev: &mut Option<TokenRef>, node: TokenRef) {
        // Link the current node to the previous node
        if let Some(prev_node) = &*prev {
            // only set right if it is not already set
            if prev_node.right().is_none() {
                prev_node.set_right(node.clone());
            }
        }
        *prev = Some(node.clone());
    }

    fn create_token(&self, id: RuleId, text: &str, prev: &Option<TokenRef>) -> TokenRef {
        self.alloc.new_token(
            id,
            text.to_string(),
            None,
            prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn create_error_token(&self, ch: char, prev: &Option<TokenRef>) -> TokenRef {
        self.alloc.new_error_token(
            RuleId::new(0), // Use a special ID for error tokens
            ch.to_string(),
            crate::tree::TokenType::ErrorUndefinedToken,
            None,
            prev.clone(),
            None, // Right will be set via link_nodes
        )
    }

    fn process_match(
        &self,
        text: &str,
        pos: &mut usize,
        prev: &mut Option<TokenRef>,
        l: usize,
        id: RuleId,
    ) -> TokenRef {
        let token_text = &text[*pos..*pos + l];
        *pos += l;
        let node = self.create_token(id, token_text, prev);
        Self::link_nodes(prev, node.clone());
        node
    }

    fn process_error(
        &self,
        _text: &str,
        pos: &mut usize,
        prev: &mut Option<TokenRef>,
        ch: char,
    ) -> TokenRef {
        *pos += ch.len_utf8();
        let node = self.create_error_token(ch, prev);
        Self::link_nodes(prev, node.clone());
        node
    }

    fn next_token(&self, text: &str, pos: &mut usize, prev: &mut Option<TokenRef>) -> TokenRef {
        let remaining = &text[*pos..];
        let mut best: Option<(usize, RuleId)> = None;

        for (id, m) in &self.matchers {
            if let Some(l) = m.match_len(remaining) {
                if l > 0 && best.map(|(bl, _)| l > bl).unwrap_or(true) {
                    best = Some((l, *id));
                }
            }
        }

        if let Some((l, id)) = best {
            return self.process_match(text, pos, prev, l, id);
        }

        let ch = remaining.chars().next().unwrap();
        self.process_error(text, pos, prev, ch)
    }

    fn next_parallel(
        &self,
        text: &str,
        pos: &mut usize,
        prev: &mut Option<TokenRef>,
        raw_rem: RawSlice,
        matcher_refs: Vec<(RuleId, MatcherRef)>,
    ) -> TokenRef {
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
            return self.process_match(text, pos, prev, l, id);
        }

        let ch = raw_rem.as_str().chars().next().unwrap();
        self.process_error(text, pos, prev, ch)
    }

    fn advance(&self, text: &str, pos: &mut usize, prev: &mut Option<TokenRef>) -> TokenRef {
        let mlen = self.matchers.len();
        if mlen <= 4 {
            // single-threaded path
            self.next_token(text, pos, prev)
        } else {
            let raw_rem = RawSlice {
                ptr: text[*pos..].as_ptr(),
                len: text[*pos..].len(),
            };
            let matcher_refs: Vec<(RuleId, MatcherRef)> = self
                .matchers
                .iter()
                .map(|(id, m)| (*id, MatcherRef(m as *const _)))
                .collect();
            self.next_parallel(text, pos, prev, raw_rem, matcher_refs)
        }
    }

    pub fn iter<'a>(&'a self, text: &'a str) -> LexIterator<'a> {
        LexIterator {
            lexer: self,
            text,
            pos: 0,
            prev: None,
        }
    }
}

// Add the LexIterator struct and its Iterator implementation
pub struct LexIterator<'a> {
    lexer: &'a Lexer,
    text: &'a str,
    pos: usize,
    prev: Option<TokenRef>,
}

impl<'a> Iterator for LexIterator<'a> {
    type Item = TokenRef;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.text.len() {
            None
        } else {
            Some(self.lexer.advance(self.text, &mut self.pos, &mut self.prev))
        }
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
        let text = "1+1*233**#***4";
        let lexer = Lexer::new(alloc.clone(), &grammar);
        let tokens: Vec<_> = lexer.iter(text).collect();
        for t in &tokens {
            print!("{}", render.pretty_tree(t.clone()));
            // use as_siblings() for left/right access
            if let Some(l) = t.left() {
                print!("\t\tL: {}", render.pretty_tree(l));
            }
            if let Some(r) = t.right() {
                println!("\t\tR: {}", render.pretty_tree(r));
            }
        }
    }
}
