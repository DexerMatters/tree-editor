// use std::sync::Arc; // removed unused import

use crate::lang::{Grammar, RuleId, RuleNode};
use crate::tree::{GreenId, NodeType, TokenRef, TreeAlloc};

pub enum Triviality {
    Trivial(Vec<GreenId>),
    NonTrivial(GreenId),
}

#[cfg(test)]
mod test {
    use tree_editor_macro::ebnf_grammar;

    use super::*;
    use crate::lexer::Lexer;
    use crate::render::Render;

    #[test]
    fn test_simple_add_parse() {
        let alloc = TreeAlloc::new();
        let grammar = ebnf_grammar! {
            expr ::= add | mul | number | "(" expr ")";
            number ::= "/[0-9]+/";
            mul ::= prec_left(10, <expr "*" expr>);
            add ::= prec_left(5, <expr "+" expr>);
        };
        let mut render = Render::new(&alloc, &grammar);
        println!("EBNF Grammar:\n{}", grammar);
        let text = "(3+()*(4+4)";
        let lexer = Lexer::new(alloc.clone(), &grammar);
        let tokens: Vec<_> = lexer.iter(text).collect();
        let parser = Parser::new(alloc.clone(), &grammar);
        let result = parser.parse(
            tokens.first().cloned(),
            grammar.get_rule_id_by_name("expr").unwrap(),
        );
        match result {
            Triviality::Trivial(greens) => {
                for g in greens {
                    let s = render.pretty_green(g);
                    println!("{}", s);
                }
            }
            Triviality::NonTrivial(g) => {
                let s = render.pretty_green(g);
                println!("{}", s);
            }
        }
    }
}

#[derive(Clone)]
struct Cursor(Option<TokenRef>);

impl Cursor {
    fn new(start: Option<TokenRef>) -> Self {
        Self(start)
    }

    fn current(&self) -> Option<TokenRef> {
        self.0.clone()
    }

    fn advance(&mut self) {
        if let Some(token) = &self.0 {
            self.0 = token.right();
        }
    }

    fn set(&mut self, pos: Option<TokenRef>) {
        self.0 = pos;
    }
}

#[derive(Debug)]
struct ParseError {
    expected: Vec<RuleId>,
    // removed `found` - the error node's children carry the concrete tokens
    consumed_tokens: Vec<GreenId>,
}

impl ParseError {
    fn new(expected: Vec<RuleId>) -> Self {
        Self {
            expected,
            consumed_tokens: Vec::new(),
        }
    }

    fn with_consumed(mut self, tokens: Vec<GreenId>) -> Self {
        self.consumed_tokens = tokens;
        self
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub struct Parser<'a> {
    pub grammar: &'a Grammar,
    pub alloc: TreeAlloc,
}

impl<'a> Parser<'a> {
    pub fn new(alloc: TreeAlloc, grammar: &'a Grammar) -> Self {
        Self { alloc, grammar }
    }

    pub fn parse(&self, start: Option<TokenRef>, start_rule: RuleId) -> Triviality {
        let mut cur = Cursor::new(start);
        match self.parse_rule(&mut cur, start_rule) {
            Ok(result) => {
                let mut greens = self.triviality_to_vec(result);

                while let Some(tok) = cur.current() {
                    // make an error node that contains this token as a child
                    let err = self.make_error_node(
                        vec![tok.green],
                        NodeType::ErrorMismatch {
                            expected: vec![start_rule],
                        },
                    );
                    greens.push(err);
                    cur.advance();
                }

                Triviality::Trivial(greens)
            }
            Err(error) => {
                let error_node = self.make_error_node(
                    error.consumed_tokens,
                    NodeType::ErrorMismatch {
                        expected: error.expected,
                    },
                );
                Triviality::Trivial(vec![error_node])
            }
        }
    }

    fn parse_rule(&self, cur: &mut Cursor, rule_id: RuleId) -> ParseResult<Triviality> {
        if let Some(parser_rule) = self.grammar.get_parser_rule(rule_id) {
            let result = self.parse_node(cur, &parser_rule.definition)?;
            Ok(self.group(Some(rule_id), result))
        } else if self.grammar.get_lexer_rule(rule_id).is_some() {
            self.parse_terminal(cur, rule_id)
        } else {
            Err(ParseError::new(vec![rule_id]))
        }
    }

    fn parse_node(&self, cur: &mut Cursor, node: &RuleNode) -> ParseResult<Vec<GreenId>> {
        match node {
            RuleNode::RuleRef(rule_id) => {
                let result = self.parse_rule(cur, *rule_id)?;
                Ok(self.triviality_to_vec(result))
            }

            RuleNode::Choice(choices) => {
                let start_pos = cur.current();
                let mut best_error: Option<ParseError> = None;

                for choice in choices {
                    cur.set(start_pos.clone());
                    match self.parse_node(cur, choice) {
                        Ok(result) => return Ok(result),
                        Err(error) => {
                            best_error = Some(match best_error {
                                None => error,
                                Some(prev) => {
                                    if self.error_made_more_progress(&error, &prev) {
                                        error
                                    } else {
                                        prev
                                    }
                                }
                            });
                        }
                    }
                }

                if let Some(error) = best_error {
                    let err_children = if !error.consumed_tokens.is_empty() {
                        error.consumed_tokens.clone()
                    } else if let Some(tok) = cur.current() {
                        vec![tok.green]
                    } else {
                        vec![]
                    };

                    let err_green = self.make_error_node(
                        err_children,
                        NodeType::ErrorMismatch {
                            expected: error.expected,
                        },
                    );

                    if cur.current().is_some() {
                        cur.advance();
                    }

                    return Ok(vec![err_green]);
                }

                let children = cur.current().map(|t| vec![t.green]).unwrap_or_default();
                let generic = self.make_error_node(
                    children.clone(),
                    NodeType::ErrorMismatch { expected: vec![] },
                );
                if cur.current().is_some() {
                    cur.advance();
                }
                Ok(vec![generic])
            }

            RuleNode::Sequence(seq) => {
                let mut results: Vec<GreenId> = Vec::new();

                for part in seq {
                    match self.parse_node(cur, part) {
                        Ok(mut tokens) => {
                            results.append(&mut tokens);
                        }
                        Err(error) => {
                            if results.is_empty() {
                                return Err(error);
                            }

                            let err_children = if !error.consumed_tokens.is_empty() {
                                error.consumed_tokens.clone()
                            } else if let Some(tok) = cur.current() {
                                vec![tok.green]
                            } else {
                                vec![]
                            };

                            let err_green = self.make_error_node(
                                err_children,
                                NodeType::ErrorMismatch {
                                    expected: error.expected,
                                },
                            );

                            results.push(err_green);
                            return Ok(results);
                        }
                    }
                }

                Ok(results)
            }

            RuleNode::Optional(inner) => {
                let start_pos = cur.current();
                match self.parse_node(cur, inner) {
                    Ok(result) => Ok(result),
                    Err(_) => {
                        cur.set(start_pos);
                        Ok(vec![])
                    }
                }
            }

            RuleNode::Repetition(inner) => {
                let mut results = Vec::new();

                loop {
                    let start_pos = cur.current();
                    match self.parse_node(cur, inner) {
                        Ok(mut tokens) => {
                            results.append(&mut tokens);
                        }
                        Err(_) => {
                            cur.set(start_pos);
                            break;
                        }
                    }
                }

                Ok(results)
            }

            RuleNode::PrecedenceMarker { operator, .. } => self.parse_node(cur, operator),
        }
    }

    fn parse_terminal(&self, cur: &mut Cursor, expected_id: RuleId) -> ParseResult<Triviality> {
        if let Some(token_ref) = cur.current() {
            let green_guard = self.alloc.get_green_by_id(token_ref.green);
            if let Some(token_rule_id) = green_guard.rule_id() {
                if token_rule_id == expected_id {
                    cur.advance();
                    return Ok(Triviality::Trivial(vec![token_ref.green]));
                } else {
                    return Err(
                        ParseError::new(vec![expected_id]).with_consumed(vec![token_ref.green])
                    );
                }
            }
            let err = self.make_error_node(
                vec![token_ref.green],
                NodeType::ErrorMismatch {
                    expected: vec![expected_id],
                },
            );
            cur.advance();
            Ok(Triviality::Trivial(vec![err]))
        } else {
            Err(ParseError::new(vec![expected_id]))
        }
    }

    fn group(&self, rule_id: Option<RuleId>, children: Vec<GreenId>) -> Triviality {
        match rule_id {
            None => Triviality::Trivial(children),
            Some(id) => {
                let name = self.grammar.name_table.get_name(id);
                if name.starts_with('_') {
                    return Triviality::Trivial(children);
                }

                // Flatten same-rule children
                let mut flat_children = Vec::new();
                for child in children {
                    if !self.alloc.is_error_green(child) {
                        let green_guard = self.alloc.get_green_by_id(child);
                        if let Some(child_rule_id) = green_guard.rule_id() {
                            if child_rule_id == id {
                                let gt: &dyn crate::tree::GreenTree = &**green_guard;
                                if let Some(node) = gt.as_node() {
                                    flat_children.extend(node.children.iter().cloned());
                                    continue;
                                }
                            }
                        }
                    }
                    flat_children.push(child);
                }

                // Collapse single non-error children
                if flat_children.len() == 1 && !self.alloc.is_error_green(flat_children[0]) {
                    Triviality::Trivial(flat_children)
                } else {
                    let node = self.alloc.new_node(id, flat_children, None);
                    Triviality::NonTrivial(node.green)
                }
            }
        }
    }

    fn triviality_to_vec(&self, triviality: Triviality) -> Vec<GreenId> {
        match triviality {
            Triviality::Trivial(vec) => vec,
            Triviality::NonTrivial(green) => vec![green],
        }
    }

    fn error_made_more_progress(&self, error1: &ParseError, error2: &ParseError) -> bool {
        error1.consumed_tokens.len() > error2.consumed_tokens.len()
    }

    fn make_error_node(&self, children: Vec<GreenId>, node_type: NodeType) -> GreenId {
        let node = self
            .alloc
            .new_error_node(RuleId::new(0), children, node_type, None);
        node.green
    }
}
