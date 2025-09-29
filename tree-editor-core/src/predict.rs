use std::sync::Arc;

use crate::{
    lang::Grammar,
    parse::LexIter,
    tree::{HasSiblings, Token, Tree, TreeAlloc, TreeRef},
};

#[derive(Debug, Clone)]
pub enum ChangeLevel {
    Trivial,
    Structural(Vec<TreeRef>),
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

    pub fn trivial_validate(&self, leaf: &Token, new_value: &str) -> ChangeLevel {
        let green_guard = leaf.intern_green(&self.alloc);
        let gt: &dyn crate::tree::GreenTree = &**green_guard;
        let old_value: Option<String> = gt.text().map(|s| s.to_string());
        let old_token_id = gt.as_token().map(|t| t.id);
        let old_hole_type = gt.as_hole().map(|h| h.hole_type);
        drop(green_guard);

        // new_value is trivial change
        if new_value.is_empty() || old_value.as_deref() == Some(new_value) {
            return ChangeLevel::Trivial;
        }

        // Re-lex using the single-threaded collector
        let mut lex_iter = LexIter::new(self.alloc.clone(), self.grammar, new_value);
        let tokens: Vec<TreeRef> = lex_iter.collect_single();

        // new_value is read as only a single token and matches the old token
        if tokens.len() == 1 {
            let tg_guard = tokens[0].intern_green(&self.alloc);
            let tg = &**tg_guard;
            let single_eq = if let Some(token) = tg.as_token() {
                old_token_id.map(|oid| oid == token.id).unwrap_or(false)
            } else if let Some(hole) = tg.as_hole() {
                old_hole_type
                    .map(|ht| ht == hole.hole_type)
                    .unwrap_or(false)
            } else {
                false
            };
            if single_eq {
                return ChangeLevel::Trivial;
            }
        }

        // new_value is read as multiple tokens but contains
        // the old token and tokens except the old token are all undefined
        if tokens.len() > 1 {
            let mut old_count = 0usize;
            for t in &tokens {
                let tg_guard = t.intern_green(&self.alloc);
                let tg = &**tg_guard;
                let is_old = if let Some(token) = tg.as_token() {
                    old_token_id.map(|oid| oid == token.id).unwrap_or(false)
                } else if let Some(hole) = tg.as_hole() {
                    old_hole_type
                        .map(|ht| ht == hole.hole_type)
                        .unwrap_or(false)
                } else {
                    false
                };
                if is_old {
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
            if let Some(sib) = first.as_siblings() {
                sib.set_left(left.clone());
            }
            if let Some(sib) = left.as_siblings() {
                sib.set_right(first.clone());
            }
        }

        if let Some(right) = leaf.right() {
            if let Some(sib) = last.as_siblings() {
                sib.set_right(right.clone());
            }
            if let Some(sib) = right.as_siblings() {
                sib.set_left(last.clone());
            }
        }

        ChangeLevel::Structural(tokens)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::tree::TreeAlloc;

    #[cfg(feature = "macros")]
    use crate::ebnf_grammar;

    #[test]
    #[cfg(feature = "macros")]
    fn test_lexer() {
        use crate::{parse::LexIter, render::Render};

        let alloc = TreeAlloc::new();
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        let mut pretty = Render::new(&alloc, &grammar);
        let validator = Validator::new(alloc.clone(), &grammar);
        let text = "1+12*30";
        let mut lexer = LexIter::new(alloc.clone(), &grammar, text);
        let tokens: Vec<_> = lexer.by_ref().collect();
        println!("{}", pretty.pretty_forest(tokens.clone()));

        // println!("--- Trivial change: '1' -> '2' ---");
        // match validator.trivial_validate(&tokens[0], "2") {
        //     ChangeLevel::Trivial => println!("Trivial change detected."),
        //     ChangeLevel::Structural(_) => println!("Structural change detected."),
        // }

        println!("--- Structural change ---");
        match validator.trivial_validate(&tokens[0].as_token().unwrap(), "12+") {
            ChangeLevel::Trivial => println!("Trivial change detected."),
            ChangeLevel::Structural(ts) => {
                println!("Structural change detected, new tokens:");
                println!("{}", pretty.pretty_forest(ts));
            }
        }
    }
}
