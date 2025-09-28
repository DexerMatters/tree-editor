use std::collections::HashMap;
use std::fmt;

use crate::utils;

/// Unique identifier for grammar rules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RuleId(u32);

impl RuleId {
    pub fn new(id: u32) -> Self {
        Self(id)
    }
}

/// Name table for efficient rule management
#[derive(Debug, Clone)]
pub struct NameTable {
    names: Vec<String>,
    name_to_id: HashMap<String, RuleId>,
    next_id: u32,
}

impl NameTable {
    pub fn new() -> Self {
        Self {
            names: Vec::new(),
            name_to_id: HashMap::new(),
            next_id: 0,
        }
    }

    pub fn intern(&mut self, name: impl Into<String>) -> RuleId {
        let name = name.into();
        if let Some(&id) = self.name_to_id.get(&name) {
            return id;
        }

        let id = RuleId::new(self.next_id);
        self.next_id += 1;
        self.names.push(name.clone());
        self.name_to_id.insert(name, id);
        id
    }

    pub fn get_name(&self, id: RuleId) -> &str {
        &self.names[id.0 as usize]
    }

    pub fn get_id(&self, name: &str) -> Option<RuleId> {
        self.name_to_id.get(name).copied()
    }
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub lexer_rules: HashMap<RuleId, LexerRule>,
    pub parser_rules: HashMap<RuleId, ParserRule>,
    pub name_table: NameTable,
    pub start_rule: RuleId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexerRule {
    pub name: RuleId,
    pub pattern: LexerPattern,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexerPattern {
    Terminal(String),
    Regex(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserRule {
    pub name: RuleId,
    pub definition: RuleNode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuleNode {
    RuleRef(RuleId),
    Choice(Vec<RuleNode>),
    Sequence(Vec<RuleNode>),
    Optional(Box<RuleNode>),
    Repetition(Box<RuleNode>),
    PrecedenceMarker {
        level: i32,
        associativity: Associativity,
        operator: Box<RuleNode>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrecedenceLevel {
    pub name: RuleId,
    pub level: i32,
    pub operator: Box<RuleNode>,
    pub associativity: Associativity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Associativity {
    Left,
    Right,
}

impl Grammar {
    pub fn new() -> Self {
        Self {
            lexer_rules: HashMap::new(),
            parser_rules: HashMap::new(),
            name_table: NameTable::new(),
            start_rule: RuleId::new(0), // Placeholder, will be set
        }
    }

    pub fn set_start_rule(&mut self, name: &str) -> RuleId {
        let id = self.name_table.intern(name);
        self.start_rule = id;
        id
    }

    pub fn add_lexer_rule(&mut self, name: &str, pattern: LexerPattern) -> RuleId {
        let id = self.name_table.intern(name);
        self.lexer_rules.insert(id, LexerRule { name: id, pattern });
        id
    }

    pub fn add_parser_rule(&mut self, name: &str, definition: RuleNode) -> RuleId {
        let id = self.name_table.intern(name);
        self.parser_rules.insert(
            id,
            ParserRule {
                name: id,
                definition,
            },
        );
        id
    }

    pub fn get_parser_rule(&self, id: RuleId) -> Option<&ParserRule> {
        self.parser_rules.get(&id)
    }

    pub fn get_lexer_rule(&self, id: RuleId) -> Option<&LexerRule> {
        self.lexer_rules.get(&id)
    }

    pub fn get_rule_name(&self, id: RuleId) -> &str {
        self.name_table.get_name(id)
    }

    pub fn process_precedence(&mut self) {
        let mut levels: Vec<PrecedenceLevel> = self
            .parser_rules
            .values()
            .filter_map(|rule| {
                if let RuleNode::PrecedenceMarker {
                    level,
                    associativity,
                    operator,
                } = &rule.definition
                {
                    Some(PrecedenceLevel {
                        name: rule.name,
                        level: *level,
                        associativity: *associativity,
                        operator: operator.clone(),
                    })
                } else {
                    None
                }
            })
            .collect();

        if levels.is_empty() {
            return;
        }

        levels.sort_by_key(|l| l.level);

        let original_start_rule_def = self
            .parser_rules
            .get(&self.start_rule)
            .unwrap()
            .definition
            .clone();

        let base_rules: Vec<RuleNode> = if let RuleNode::Choice(choices) = original_start_rule_def {
            choices
                .into_iter()
                .filter(
                    |c| !matches!(c, RuleNode::RuleRef(id) if levels.iter().any(|l| l.name == *id)),
                )
                .collect()
        } else {
            vec![original_start_rule_def]
        };

        let factor_rule_name = self.name_table.intern("_factor");
        self.parser_rules.insert(
            factor_rule_name,
            ParserRule {
                name: factor_rule_name,
                definition: RuleNode::Choice(base_rules),
            },
        );

        let mut current_level_rule = RuleNode::RuleRef(factor_rule_name);

        let mut level_groups: Vec<Vec<PrecedenceLevel>> = Vec::new();
        if !levels.is_empty() {
            level_groups.push(vec![levels[0].clone()]);
            for i in 1..levels.len() {
                if levels[i].level == levels[i - 1].level {
                    level_groups.last_mut().unwrap().push(levels[i].clone());
                } else {
                    level_groups.push(vec![levels[i].clone()]);
                }
            }
        }

        for group in level_groups.iter().rev() {
            let level = group[0].level;
            let new_level_name = self.create_precedence_rule_name(level);

            let operators: Vec<RuleNode> = group
                .iter()
                .map(|item| {
                    if let RuleNode::Sequence(nodes) = &*item.operator {
                        // Assuming the operator is the middle element in a sequence like `expr OP expr`
                        if nodes.len() == 3 {
                            return nodes[1].clone();
                        }
                    }
                    // Fallback for other structures
                    (*item.operator).clone()
                })
                .collect();

            let op_choice = if operators.len() > 1 {
                RuleNode::Choice(operators)
            } else {
                operators[0].clone()
            };

            let new_rule = match group[0].associativity {
                Associativity::Left => seq(vec![
                    current_level_rule.clone(),
                    repeat(seq(vec![op_choice, current_level_rule.clone()])),
                ]),
                Associativity::Right => seq(vec![
                    current_level_rule.clone(),
                    optional(seq(vec![op_choice, RuleNode::RuleRef(new_level_name)])),
                ]),
            };

            self.parser_rules.insert(
                new_level_name,
                ParserRule {
                    name: new_level_name,
                    definition: new_rule,
                },
            );
            current_level_rule = RuleNode::RuleRef(new_level_name);
        }

        if let Some(start) = self.parser_rules.get_mut(&self.start_rule) {
            start.definition = current_level_rule;
        }

        // Remove the original precedence marker rules
        for level in levels {
            self.parser_rules.remove(&level.name);
        }
    }

    fn create_precedence_rule_name(&mut self, level: i32) -> RuleId {
        let name = format!("_prec{}", level);
        self.name_table.intern(name)
    }

    /// Get or create a rule ID for a given name (for macro convenience)
    pub fn get_or_create_rule_id(&mut self, name: &str) -> RuleId {
        if let Some(id) = self.name_table.get_id(name) {
            id
        } else {
            self.name_table.intern(name)
        }
    }

    /// Immutable rule ref (names must be pre-interned beforehand)
    pub fn rule_ref(&self, name: &str) -> RuleNode {
        let id = self
            .name_table
            .get_id(name)
            .expect("rule name not interned");
        RuleNode::RuleRef(id)
    }
}

// Helper functions for creating grammar rules
pub fn rule_ref(name: &str, name_table: &mut NameTable) -> RuleNode {
    let id = name_table.intern(name);
    RuleNode::RuleRef(id)
}

pub fn choice(alternatives: Vec<RuleNode>) -> RuleNode {
    match alternatives.len() {
        0 => panic!("Empty choice not allowed"),
        1 => alternatives.into_iter().next().unwrap(),
        _ => RuleNode::Choice(alternatives),
    }
}

pub fn seq(elements: Vec<RuleNode>) -> RuleNode {
    match elements.len() {
        0 => {
            // Return an empty sequence instead of panicking
            RuleNode::Sequence(vec![])
        }
        1 => elements.into_iter().next().unwrap(),
        _ => RuleNode::Sequence(elements),
    }
}

pub fn optional(node: RuleNode) -> RuleNode {
    RuleNode::Optional(Box::new(node))
}

pub fn repeat(node: RuleNode) -> RuleNode {
    RuleNode::Repetition(Box::new(node))
}

/// Create precedence marker for grammar building
pub fn prec_left(level: i32, operator: RuleNode) -> RuleNode {
    RuleNode::PrecedenceMarker {
        level,
        associativity: Associativity::Left,
        operator: Box::new(operator),
    }
}
pub fn prec_right(level: i32, operator: RuleNode) -> RuleNode {
    RuleNode::PrecedenceMarker {
        level,
        associativity: Associativity::Right,
        operator: Box::new(operator),
    }
}

/// Wrapper to provide grammar context to Display implementations
pub struct DisplayWithGrammar<'a, T> {
    grammar: &'a Grammar,
    node: &'a T,
}

impl<'a, T> DisplayWithGrammar<'a, T> {
    pub fn new(grammar: &'a Grammar, node: &'a T) -> Self {
        Self { grammar, node }
    }
}

impl fmt::Display for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

// Display implementations - optimized for better performance
impl<'a> fmt::Display for DisplayWithGrammar<'a, RuleNode> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let grammar = self.grammar;
        match self.node {
            RuleNode::RuleRef(id) => {
                if let Some(lexer_rule) = grammar.lexer_rules.get(id) {
                    match &lexer_rule.pattern {
                        LexerPattern::Terminal(s) => {
                            write!(f, "{}\"{}\"{}", utils::TERMINAL, s, utils::RESET)
                        }
                        LexerPattern::Regex(r) => {
                            write!(f, "{}/{}/{}", utils::TERMINAL, r, utils::RESET)
                        }
                    }
                } else {
                    write!(
                        f,
                        "{}{}{}",
                        utils::RULE_REF,
                        grammar.name_table.get_name(*id),
                        utils::RESET
                    )
                }
            }
            RuleNode::Choice(nodes) => {
                write!(f, "{}({}", utils::BRACKET, utils::RESET)?;
                for (i, n) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, " {}|{} ", utils::OPERATOR, utils::RESET)?;
                    }
                    write!(f, "{}", DisplayWithGrammar::new(grammar, n))?;
                }
                write!(f, "{}){}", utils::BRACKET, utils::RESET)
            }
            RuleNode::Sequence(nodes) => {
                for (i, n) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", DisplayWithGrammar::new(grammar, n))?;
                }
                Ok(())
            }
            RuleNode::Optional(n) => {
                if matches!(**n, RuleNode::Choice(_) | RuleNode::Sequence(_)) {
                    write!(
                        f,
                        "{}[{}{}{}]?{}",
                        utils::BRACKET,
                        utils::RESET,
                        DisplayWithGrammar::new(grammar, &**n),
                        utils::BRACKET,
                        utils::RESET
                    )
                } else {
                    write!(
                        f,
                        "{}{}{}?{}",
                        utils::RESET,
                        DisplayWithGrammar::new(grammar, &**n),
                        utils::OPERATOR,
                        utils::RESET
                    )
                }
            }
            RuleNode::Repetition(n) => {
                if matches!(**n, RuleNode::Choice(_) | RuleNode::Sequence(_)) {
                    write!(
                        f,
                        "{}({}{}{})*{}",
                        utils::BRACKET,
                        utils::RESET,
                        DisplayWithGrammar::new(grammar, &**n),
                        utils::BRACKET,
                        utils::RESET
                    )
                } else {
                    write!(
                        f,
                        "{}{}{}*{}",
                        utils::RESET,
                        DisplayWithGrammar::new(grammar, &**n),
                        utils::OPERATOR,
                        utils::RESET
                    )
                }
            }
            RuleNode::PrecedenceMarker {
                level,
                associativity,
                operator,
            } => {
                let assoc = match associativity {
                    Associativity::Left => "L",
                    Associativity::Right => "R",
                };
                write!(
                    f,
                    "<PREC {} {} {}>",
                    level,
                    assoc,
                    DisplayWithGrammar::new(grammar, &**operator)
                )
            }
        }
    }
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Display lexer rules first
        if !self.lexer_rules.is_empty() {
            writeln!(f, "{}// Lexer Rules{}", utils::RULE_NAME, utils::RESET)?;
            let mut lexer_ids: Vec<_> = self.lexer_rules.keys().collect();
            lexer_ids.sort();
            for &id in lexer_ids {
                if let Some(rule) = self.lexer_rules.get(&id) {
                    let name = self.name_table.get_name(id);
                    // Hide internal lexer rules from display
                    if name.starts_with("__") {
                        continue;
                    }
                    match &rule.pattern {
                        LexerPattern::Terminal(t) => {
                            writeln!(
                                f,
                                "{}{}{} {}::={} {}\"{}\"{}",
                                utils::RULE_NAME,
                                name,
                                utils::RESET,
                                utils::OPERATOR,
                                utils::RESET,
                                utils::TERMINAL,
                                t,
                                utils::RESET
                            )?;
                        }
                        LexerPattern::Regex(r) => {
                            writeln!(
                                f,
                                "{}{}{} {}::={} {}/{}{}",
                                utils::RULE_NAME,
                                name,
                                utils::RESET,
                                utils::OPERATOR,
                                utils::RESET,
                                utils::TERMINAL,
                                r,
                                utils::RESET
                            )?;
                        }
                    }
                }
            }
            writeln!(f)?;
        }

        // Display parser rules
        if !self.parser_rules.is_empty() {
            writeln!(f, "{}// Parser Rules{}", utils::RULE_NAME, utils::RESET)?;
            let mut parser_ids: Vec<_> = self.parser_rules.keys().collect();
            parser_ids.sort();
            for &id in parser_ids {
                if let Some(rule) = self.parser_rules.get(&id) {
                    let name = self.name_table.get_name(id);
                    writeln!(
                        f,
                        "{}{}{} {}::={} {}",
                        utils::RULE_NAME,
                        name,
                        utils::RESET,
                        utils::OPERATOR,
                        utils::RESET,
                        DisplayWithGrammar::new(self, &rule.definition)
                    )?;
                }
            }
        }
        Ok(())
    }
}
