use std::{collections::HashMap, fmt::format};

use crate::{
    lang::{Grammar, RuleId},
    tree::{GreenId, GreenTree, HoleType, Tree, TreeAlloc},
    utils,
};

pub struct Render<'a> {
    pub grammar: &'a Grammar,
    pub alloc: &'a TreeAlloc,
    pub memo: HashMap<(u8, GreenId), String>, // (indent_level, green_id) -> pretty string
}

impl<'a> Render<'a> {
    pub fn new(alloc: &'a TreeAlloc, grammar: &'a Grammar) -> Self {
        Self {
            alloc,
            grammar,
            memo: HashMap::new(),
        }
    }
    pub fn pretty_tree(&mut self, tree: impl AsRef<Tree>) -> String {
        let green_id = tree.as_ref().green();
        self.pretty_green(0, green_id)
    }
    fn pretty_green(&mut self, indent_level: u8, green_id: GreenId) -> String {
        match &*self.alloc.get_green_by_id(green_id).unwrap() {
            GreenTree::Node { id, children, .. } => {
                self.pretty_node(indent_level, *id, children, green_id)
            }
            GreenTree::Token { id, text, .. } => {
                // pass borrowed &str to avoid cloning the token text
                self.pretty_token(indent_level, *id, text.as_str(), green_id)
            }
            GreenTree::Hole {
                text, hole_type, ..
            } => {
                // pass borrowed &str to avoid cloning the hole text
                self.pretty_hole(indent_level, text.as_str(), *hole_type, green_id)
            }
        }
    }
    fn pretty_node(
        &mut self,
        indent_level: u8,
        rule_id: RuleId,
        children: &Vec<GreenId>,
        green_id: GreenId,
    ) -> String {
        if let Some(s) = self.memo.get(&(indent_level, green_id)) {
            return s.clone();
        }

        let indent = "  ".repeat(indent_level as usize);
        let rule_name = format!(
            "{}{}{}",
            utils::RULE_NAME,
            self.grammar.get_rule_name(rule_id),
            utils::RESET,
        );
        let mut result = format!("{}{}:\n", indent, rule_name);
        for child_id in children {
            let child_str = self.pretty_green(indent_level + 1, *child_id);
            result.push_str(&child_str);
            result.push('\n');
        }
        result.pop(); // remove last newline
        self.memo.insert((indent_level, green_id), result.clone());
        result
    }
    // changed to borrow token text
    fn pretty_token(
        &mut self,
        indent_level: u8,
        rule_id: RuleId,
        text: &str,
        green_id: GreenId,
    ) -> String {
        if let Some(s) = self.memo.get(&(indent_level, green_id)) {
            return s.clone();
        }

        let indent = "  ".repeat(indent_level as usize);
        let rule_name = format!(
            "{}{}{}",
            utils::TERMINAL,
            self.grammar.get_rule_name(rule_id),
            utils::RESET,
        );
        let token_text = format!("{}\"{}\"{}", utils::BRACKET, text, utils::RESET);
        let result = format!("{}{}: {}", indent, rule_name, token_text);
        self.memo.insert((indent_level, green_id), result.clone());
        result
    }
    // changed to borrow hole text and build hole_type_str as String
    fn pretty_hole(
        &mut self,
        indent_level: u8,
        text: &str,
        hole_type: HoleType,
        green_id: GreenId,
    ) -> String {
        if let Some(s) = self.memo.get(&(indent_level, green_id)) {
            return s.clone();
        }

        let indent = "  ".repeat(indent_level as usize);
        let hole_text = format!("{}[{}]{}", utils::ERROR, text, utils::RESET);
        let hole_type_str = match hole_type {
            HoleType::Incomplete => "Incomplete".to_string(),
            HoleType::UndefinedToken => "UndefinedToken".to_string(),
            HoleType::Mismatch { expected } => {
                let expected_name = self.grammar.get_rule_name(expected);
                format!("Mismatch (expected {})", expected_name)
            }
        };
        let result = format!("{}Hole ({}): {}", indent, hole_type_str, hole_text);
        self.memo.insert((indent_level, green_id), result.clone());
        result
    }
}
