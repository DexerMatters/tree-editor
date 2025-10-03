use crate::{
    lang::Grammar,
    lexer::Lexer,
    tree::{GreenLeaf, TokenRef, Tree, TreeAlloc},
};

#[derive(Debug, Clone)]
pub enum ChangeLevel {
    Trivial,
    Explosion(Vec<TokenRef>),
}

#[derive(Debug, Clone)]
pub struct Validator<'a> {
    pub grammar: &'a Grammar,
    pub alloc: TreeAlloc,
    pub lexer: &'a Lexer,
}

impl<'a> Validator<'a> {
    pub fn new(alloc: TreeAlloc, grammar: &'a Grammar, lexer: &'a Lexer) -> Self {
        Self {
            alloc: alloc.clone(),
            grammar,
            lexer,
        }
    }

    pub fn lexically_trivial_validate(&self, leaf: &TokenRef, new_value: &str) -> ChangeLevel {
        let green_guard = leaf.intern_green(&self.alloc);
        let green = green_guard.as_token().unwrap();
        let old_value = green.text().to_owned();
        let old_token_id = green.id;
        drop(green_guard);

        // new_value is trivial change
        if new_value.is_empty() || old_value == new_value {
            return ChangeLevel::Trivial;
        }

        let tokens = self.lexer.tokenize(new_value);

        // new_value is read as only a single token and matches the old token
        if tokens.len() == 1 {
            let new_green = tokens[0].intern_green(&self.alloc);
            let single_eq = if let Some(token) = new_green.as_token() {
                old_token_id == token.id
            } else {
                false
            };
            if single_eq {
                return ChangeLevel::Trivial;
            }
        }

        // new_value is read as multiple tokens but contains
        // the old token and tokens except the old token are all error tokens
        if tokens.len() > 1 {
            let mut old_count = 0usize;
            for t in &tokens {
                let tg = t.intern_green(&self.alloc);
                let is_old = if let Some(token) = tg.as_token() {
                    old_token_id == token.id
                } else {
                    false
                };
                if is_old {
                    old_count += 1;
                } else if tg.is_error_token() {
                    // allowed error token
                } else {
                    // found a token that's neither the old token nor an error token
                    return ChangeLevel::Explosion(tokens);
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

        ChangeLevel::Explosion(tokens)
    }
    pub fn structurally_trivial_validate(&self, _new_tokens: Vec<TokenRef>) -> ChangeLevel {
        // Placeholder implementation
        ChangeLevel::Trivial
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
        use crate::{lexer::Lexer, render::Render};

        let alloc = TreeAlloc::new();
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        let mut pretty = Render::new(&alloc, &grammar);
        let lexer = Lexer::new(alloc.clone(), &grammar);
        let validator = Validator::new(alloc.clone(), &grammar, &lexer);
        let text = "1+12*30";
        let tokens: Vec<_> = lexer.tokenize(text);
        println!("{}", pretty.pretty_forest(tokens.clone()));

        // println!("--- Trivial change: '1' -> '2' ---");
        // match validator.trivial_validate(&tokens[0], "2") {
        //     ChangeLevel::Trivial => println!("Trivial change detected."),
        //     ChangeLevel::Structural(_) => println!("Structural change detected."),
        // }

        println!("--- Structural change ---");
        match validator.lexically_trivial_validate(&tokens[0], "12+") {
            ChangeLevel::Trivial => println!("Trivial change detected."),
            ChangeLevel::Explosion(ts) => {
                println!("Structural change detected, new tokens:");
                println!("{}", pretty.pretty_forest(ts));
            }
        }
    }
}
