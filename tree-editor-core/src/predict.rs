use std::sync::Arc;

use regex::Regex;

use crate::{
    lang::{Grammar, RuleNode},
    parse::LexIter,
    tree::{GreenTree, Tree, TreeAlloc},
};

#[derive(Debug, Clone)]
pub enum ChangeLevel {
    Trivial,
    Structural(Vec<Arc<Tree>>),
}

#[derive(Debug, Clone)]
pub struct Validator<'a> {
    pub grammar: &'a Grammar,
    pub alloc: TreeAlloc,
}

impl<'a> Validator<'a> {
    pub fn new(alloc: TreeAlloc, grammar: &'a Grammar) -> Self {
        Self { alloc, grammar }
    }

    pub fn trivial_validate(&self, leaf: &Arc<Tree>, new_value: &str) -> ChangeLevel {
        let green = leaf.intern_green(&self.alloc).unwrap();
        let old_value = green.text().unwrap();
        // new_value is trivial change
        if new_value.is_empty() || new_value == old_value {
            return ChangeLevel::Trivial;
        }

        // Re-lex
        let lex_iter = LexIter::new(self.alloc.clone(), self.grammar, new_value);
        let tokens: Vec<_> = lex_iter.collect();

        // new_value is read as only a single token and matches the old token
        if tokens.len() == 1
            && tokens[0]
                .intern_green(&self.alloc)
                .unwrap()
                .lexical_equal(&green)
        // Rule unchanged
        {
            return ChangeLevel::Trivial;
        }

        // new_value is read as multiple tokens but contains
        // the old token and tokens except the old token are all undefined
        if tokens.len() > 1 {
            let mut old_count = 0usize;
            for t in &tokens {
                let tg = t.intern_green(&self.alloc).unwrap();
                if tg.lexical_equal(&green) {
                    old_count += 1;
                } else if tg.is_undefined_token() {
                    // allowed undefined token
                } else {
                    // found a token that's neither the old token nor an undefined token
                    return ChangeLevel::Structural(tokens);
                }
            }
            if old_count == 1 {
                return ChangeLevel::Trivial;
            }
        }

        // The change is non-trivial. Join the newly lexed tokens into the context
        let first = tokens.first().unwrap();
        let last = tokens.last().unwrap();
        if let Some(left) = leaf.left() {
            first.set_left(left.clone());
            left.set_right(first.clone());
        }

        if let Some(right) = leaf.right() {
            last.set_right(right.clone());
            right.set_left(last.clone());
        }

        return ChangeLevel::Structural(tokens);
    }
}
