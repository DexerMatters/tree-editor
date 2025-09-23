use std::collections::HashMap;
use std::fmt;

// ANSI color codes
const RESET: &str = "\x1b[0m";
const RULE_NAME: &str = "\x1b[1;34m"; // Bold Blue
const TERMINAL: &str = "\x1b[32m"; // Green
const RULE_REF: &str = "\x1b[33m"; // Yellow
const OPERATOR: &str = "\x1b[1;37m"; // Bold White
const BRACKET: &str = "\x1b[35m"; // Magenta

// Precedence system constants
const PREC_MARKER_PREFIX: &str = "__PREC_";
const PREC_MARKER_SUFFIX: &str = "__";

#[derive(Debug, Clone, PartialEq)]
pub struct Grammar<'a> {
    pub rules: HashMap<&'a str, Rule<'a>>,
    pub start_rule: &'a str,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Rule<'a> {
    pub name: &'a str,
    pub definition: RuleNode<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum RuleNode<'a> {
    RuleRef(&'a str),
    Terminal(&'a str),
    Choice(Vec<RuleNode<'a>>),
    Sequence(Vec<RuleNode<'a>>),
    Optional(Box<RuleNode<'a>>),
    Repetition(Box<RuleNode<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PrecedenceLevel<'a> {
    pub name: &'a str,
    pub level: i32,
    pub operator: RuleNode<'a>,
    pub associativity: Associativity,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

impl<'a> Grammar<'a> {
    pub fn new(start_rule: &'a str) -> Self {
        Self {
            rules: HashMap::new(),
            start_rule,
        }
    }

    pub fn add_rule(&mut self, name: &'a str, definition: RuleNode<'a>) {
        self.rules.insert(name, Rule { name, definition });
    }

    pub fn process_precedence(&mut self) {
        let mut precedence_levels = self.extract_precedence_levels();
        if precedence_levels.is_empty() {
            return;
        }

        // Sort by level, highest first
        precedence_levels.sort_by_key(|p| -p.level);

        let base_rule = self.find_base_rule(&precedence_levels);
        self.generate_precedence_chain(precedence_levels, base_rule);
    }

    pub fn get_rule(&self, name: &str) -> Option<&Rule<'a>> {
        self.rules.get(name)
    }

    /// Extract precedence information in one clean pass
    fn extract_precedence_levels(&self) -> Vec<PrecedenceLevel<'a>> {
        self.rules
            .values()
            .filter_map(|rule| self.parse_precedence_marker(rule.name, &rule.definition))
            .collect()
    }

    fn parse_precedence_marker(
        &self,
        name: &'a str,
        node: &RuleNode<'a>,
    ) -> Option<PrecedenceLevel<'a>> {
        if let RuleNode::Sequence(seq) = node {
            if seq.len() == 2 {
                if let RuleNode::Terminal(marker) = &seq[0] {
                    if let RuleNode::Repetition(rep) = &seq[1] {
                        return self.decode_precedence_marker(name, marker, rep);
                    }
                }
            }
        }
        None
    }

    /// Decode precedence markers with better error handling
    fn decode_precedence_marker(
        &self,
        name: &'a str,
        marker: &str,
        rep: &Box<RuleNode<'a>>,
    ) -> Option<PrecedenceLevel<'a>> {
        if !marker.starts_with(PREC_MARKER_PREFIX) || !marker.ends_with(PREC_MARKER_SUFFIX) {
            return None;
        }

        let parts: Vec<_> = marker
            [PREC_MARKER_PREFIX.len()..marker.len() - PREC_MARKER_SUFFIX.len()]
            .split('_')
            .collect();
        if parts.len() != 2 {
            return None;
        }

        let level = parts[0].parse().ok()?;
        let assoc = match parts[1] {
            "L" => Associativity::Left,
            "R" => Associativity::Right,
            _ => return None,
        };

        if let RuleNode::Sequence(inner) = rep.as_ref() {
            // The operator is the sequence inside the <...>
            Some(PrecedenceLevel {
                name,
                level,
                operator: RuleNode::Sequence(inner.clone()),
                associativity: assoc,
            })
        } else {
            None
        }
    }

    /// Find base rule more efficiently
    fn find_base_rule(&self, _precedence_levels: &[PrecedenceLevel<'a>]) -> RuleNode<'a> {
        let Some(main_rule) = self.rules.get(self.start_rule) else {
            return self.default_base_rule();
        };

        match &main_rule.definition {
            RuleNode::Choice(alts) => {
                let base_alts: Vec<_> = alts
                    .iter()
                    .filter(|alt| !self.is_precedence_marker_alt(alt))
                    .cloned()
                    .collect();

                match base_alts.len() {
                    0 => self.find_fallback_base_rule(),
                    1 => base_alts[0].clone(),
                    _ => RuleNode::Choice(base_alts),
                }
            }
            other => other.clone(),
        }
    }

    fn is_precedence_marker_alt(&self, alt: &RuleNode<'a>) -> bool {
        if let RuleNode::RuleRef(name) = alt {
            if let Some(rule) = self.rules.get(name) {
                if let RuleNode::Sequence(seq) = &rule.definition {
                    return seq.len() == 2
                        && matches!(&seq[0], RuleNode::Terminal(marker) if marker.starts_with(PREC_MARKER_PREFIX));
                }
            }
        }
        false
    }

    fn default_base_rule(&self) -> RuleNode<'a> {
        RuleNode::RuleRef("number")
    }

    fn find_fallback_base_rule(&self) -> RuleNode<'a> {
        let candidates: Vec<_> = self
            .rules
            .keys()
            .filter(|&name| *name != self.start_rule && !name.starts_with('_'))
            .map(|&name| RuleNode::RuleRef(name))
            .collect();

        match candidates.len() {
            0 => self.default_base_rule(),
            1 => candidates[0].clone(),
            _ => RuleNode::Choice(candidates),
        }
    }

    /// Generate precedence chain with cleaner logic
    fn generate_precedence_chain(
        &mut self,
        mut levels: Vec<PrecedenceLevel<'a>>,
        base_rule: RuleNode<'a>,
    ) {
        levels.sort_by(|a, b| b.level.cmp(&a.level)); // Highest precedence first

        // The first level of precedence builds on the base rule
        let mut next_level_rule = base_rule;

        // Group levels by precedence value
        let mut level_groups: Vec<(i32, Vec<PrecedenceLevel<'a>>)> = Vec::new();
        for level in levels {
            if let Some(last) = level_groups.last_mut() {
                if last.0 == level.level {
                    last.1.push(level);
                    continue;
                }
            }
            level_groups.push((level.level, vec![level]));
        }

        // Build the chain from lowest precedence up to highest
        for (_level, group) in level_groups.iter().rev() {
            let mut choices = Vec::new();
            for prec_def in group {
                let choice = self.create_precedence_choice(prec_def, &next_level_rule);
                // Create a new rule for this specific precedence operator
                self.add_rule(prec_def.name, choice);
                choices.push(RuleNode::RuleRef(prec_def.name));
            }
            // Also allow falling through to the next level of precedence
            choices.push(next_level_rule.clone());

            let rule_name = self.create_precedence_rule_name(group[0].level);
            let rule_def = RuleNode::Choice(choices);
            self.add_rule(rule_name, rule_def);
            next_level_rule = RuleNode::RuleRef(rule_name);
        }

        // Update start rule to point to the top of the precedence chain
        if let Some(rule) = self.rules.get_mut(self.start_rule) {
            rule.definition = next_level_rule;
        }

        // Original precedence rules are now replaced, so no need to remove them.
    }

    fn create_precedence_choice(
        &self,
        prec_def: &PrecedenceLevel<'a>,
        next_level_rule: &RuleNode<'a>,
    ) -> RuleNode<'a> {
        let mut new_op = prec_def.operator.clone();
        let start_rule_name = self.start_rule;

        // Replace recursive references in the operator with the next level rule
        if let RuleNode::Sequence(nodes) = &mut new_op {
            for node in nodes.iter_mut() {
                if let RuleNode::RuleRef(name) = node {
                    if *name == start_rule_name {
                        *node = next_level_rule.clone();
                    }
                }
            }
        }
        new_op
    }

    fn create_precedence_rule_name(&self, level: i32) -> &'a str {
        let name = format!("_prec{}", level);
        Box::leak(name.into_boxed_str())
    }
}

// Helper functions - more concise and elegant
pub fn terminal<'a>(text: &'a str) -> RuleNode<'a> {
    RuleNode::Terminal(text)
}

pub fn rule<'a>(name: &'a str) -> RuleNode<'a> {
    RuleNode::RuleRef(name)
}

pub fn choice<'a>(alternatives: Vec<RuleNode<'a>>) -> RuleNode<'a> {
    match alternatives.len() {
        0 => panic!("Empty choice not allowed"),
        1 => alternatives.into_iter().next().unwrap(),
        _ => RuleNode::Choice(alternatives),
    }
}

pub fn seq<'a>(elements: Vec<RuleNode<'a>>) -> RuleNode<'a> {
    match elements.len() {
        0 => panic!("Empty sequence not allowed"),
        1 => elements.into_iter().next().unwrap(),
        _ => RuleNode::Sequence(elements),
    }
}

pub fn optional<'a>(node: RuleNode<'a>) -> RuleNode<'a> {
    RuleNode::Optional(Box::new(node))
}

pub fn repeat<'a>(node: RuleNode<'a>) -> RuleNode<'a> {
    RuleNode::Repetition(Box::new(node))
}

pub fn regex<'a>(pattern: &'a str) -> RuleNode<'a> {
    RuleNode::Terminal(pattern)
}

/// Elegant precedence functions
pub fn prec_left<'a>(level: i32, operator: RuleNode<'a>) -> RuleNode<'a> {
    create_precedence_marker(level, operator, Associativity::Left)
}

pub fn prec_right<'a>(level: i32, operator: RuleNode<'a>) -> RuleNode<'a> {
    create_precedence_marker(level, operator, Associativity::Right)
}

fn create_precedence_marker<'a>(
    level: i32,
    operator: RuleNode<'a>,
    associativity: Associativity,
) -> RuleNode<'a> {
    let assoc_str = match associativity {
        Associativity::Left => "L",
        Associativity::Right => "R",
    };

    let marker_str = format!(
        "{}{}_{}{}",
        PREC_MARKER_PREFIX, level, assoc_str, PREC_MARKER_SUFFIX
    );
    let marker_terminal = RuleNode::Terminal(Box::leak(marker_str.into_boxed_str()));

    // This structure is specifically for the extract_precedence_levels function to parse.
    RuleNode::Sequence(vec![
        marker_terminal,
        RuleNode::Repetition(Box::new(operator)),
    ])
}

#[macro_export]
macro_rules! make_grammar {
    ($start:ident, $($rule_name:ident ::= $rule:expr),+ $(,)?) => {
        {
            use $crate::lang::Grammar;
            let mut _grammar = Grammar::new(stringify!($start));
            $(
                _grammar.add_rule(stringify!($rule_name), $rule);
            )+
            _grammar
        }
    };
}

#[macro_export]
macro_rules! seq {
    ($($elem:expr),+ $(,)?) => {
        $crate::lang::seq(vec![$($elem),+])
    };
}

#[macro_export]
macro_rules! choice {
    ($($elem:expr),+ $(,)?) => {
        $crate::lang::choice(vec![$($elem),+])
    };
}

#[macro_export]
macro_rules! opt {
    ($elem:expr) => {
        $crate::lang::optional($elem)
    };
}

#[macro_export]
macro_rules! rep {
    ($elem:expr) => {
        $crate::lang::repeat($elem)
    };
}

#[macro_export]
macro_rules! term {
    ($literal:literal) => {
        $crate::lang::terminal($literal)
    };
}

#[macro_export]
macro_rules! rule {
    ($ident:ident) => {
        $crate::lang::rule(stringify!($ident))
    };
}

// Display implementations - optimized for better performance
impl<'a> fmt::Display for RuleNode<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuleNode::RuleRef(n) => write!(f, "{}{}{}", RULE_REF, n, RESET),
            RuleNode::Terminal(t) => {
                if t.starts_with('/') && t.ends_with('/') && t.len() > 2 {
                    write!(f, "{}{}{}", TERMINAL, t, RESET)
                } else {
                    write!(f, "{}\"{}\"{}", TERMINAL, t, RESET)
                }
            }
            RuleNode::Choice(nodes) => {
                write!(f, "{}({}", BRACKET, RESET)?;
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, " {}|{} ", OPERATOR, RESET)?;
                    }
                    write!(f, "{}", node)?;
                }
                write!(f, "{}){}", BRACKET, RESET)
            }
            RuleNode::Sequence(nodes) => {
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", node)?;
                }
                Ok(())
            }
            RuleNode::Optional(n) => {
                if matches!(**n, RuleNode::Choice(_) | RuleNode::Sequence(_)) {
                    write!(f, "{}[{}{}{}]?{}", BRACKET, RESET, n, BRACKET, RESET)
                } else {
                    write!(f, "{}{}{}?{}", RESET, n, OPERATOR, RESET)
                }
            }
            RuleNode::Repetition(n) => {
                if matches!(**n, RuleNode::Choice(_) | RuleNode::Sequence(_)) {
                    write!(f, "{}({}{}{})*{}", BRACKET, RESET, n, BRACKET, RESET)
                } else {
                    write!(f, "{}{}{}*{}", RESET, n, OPERATOR, RESET)
                }
            }
        }
    }
}

impl<'a> fmt::Display for Rule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}{} {}::={} {}",
            RULE_NAME, self.name, RESET, OPERATOR, RESET, self.definition
        )
    }
}

impl<'a> fmt::Display for Grammar<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut names: Vec<&str> = self.rules.keys().copied().collect();
        names.sort_unstable();

        for name in names {
            if let Some(rule) = self.rules.get(name) {
                writeln!(f, "{}", rule)?;
            }
        }
        Ok(())
    }
}
