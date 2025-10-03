use std::{collections::HashMap, sync::Arc};

use crate::{
    lang::{Grammar, RuleId},
    tree::{GreenId, GreenTree, NodeType, TokenType, Tree, TreeAlloc},
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
    pub fn pretty_tree(&mut self, tree: Arc<impl Tree + ?Sized>) -> String {
        let green_id = tree.as_ref().green();
        self._pretty_green(0, green_id)
    }
    pub fn pretty_green(&mut self, green_id: GreenId) -> String {
        self._pretty_green_grouped(green_id, 0)
    }

    pub fn pretty_green_path(&mut self, green_id: GreenId) -> String {
        self._pretty_green_path(vec![], green_id)
    }

    /// Render nodes grouped by parent-child relationships with indentation
    fn _pretty_green_grouped(&mut self, green_id: GreenId, indent: usize) -> String {
        let guard = self.alloc.get_green_by_id(green_id);
        let gt: &dyn GreenTree = &**guard;

        let indent_str = "  ".repeat(indent);

        if let Some(node) = gt.as_node() {
            self.pretty_node_grouped(indent_str, node.id, &node.children, &node.node_type, indent)
        } else if let Some(token) = gt.as_token() {
            self.pretty_token_grouped(indent_str, token.id, token.text.as_str(), &token.token_type)
        } else {
            String::new()
        }
    }

    // Helper: join expected rule names with '|'
    fn expected_names(&self, expected: &Vec<RuleId>) -> String {
        expected
            .iter()
            .map(|r| self.grammar.get_rule_name(*r))
            .collect::<Vec<_>>()
            .join("|")
    }

    // Helper: color a text by depth (usize) using DEPTH_COLORS
    fn color_by_depth(&self, depth: usize, text: &str) -> String {
        let color = utils::DEPTH_COLORS
            .get(depth)
            .unwrap_or(&utils::DEPTH_COLORS[depth % utils::DEPTH_COLORS.len()]);
        format!("{}{}{}", color, text, utils::RESET)
    }

    fn pretty_node_grouped(
        &mut self,
        indent_str: String,
        rule_id: RuleId,
        children: &Vec<GreenId>,
        node_type: &NodeType,
        indent: usize,
    ) -> String {
        let rule_name = self.grammar.get_rule_name(rule_id);
        let colored_rule_name = self.color_by_depth(indent, rule_name);

        let error_marker = match node_type {
            NodeType::Regular => String::new(),
            NodeType::ErrorIncomplete => format!(" {}[INCOMPLETE]{}", utils::ERROR, utils::RESET),
            NodeType::ErrorMismatch { expected } => {
                format!(
                    " {}[ERROR: expected {}]{}",
                    utils::ERROR,
                    self.expected_names(expected),
                    utils::RESET
                )
            }
            NodeType::ErrorUnexpectedEOF => {
                format!(" {}[UNEXPECTED_EOF]{}", utils::ERROR, utils::RESET)
            }
        };

        let mut result = format!("{}{}{}", indent_str, colored_rule_name, error_marker);

        if !children.is_empty() {
            result.push_str(":");
            for child_id in children {
                let child_str = self._pretty_green_grouped(*child_id, indent + 1);
                if !child_str.trim().is_empty() {
                    result.push('\n');
                    result.push_str(&child_str);
                }
            }
        }
        result
    }

    fn pretty_token_grouped(
        &mut self,
        indent_str: String,
        rule_id: RuleId,
        text: &str,
        token_type: &TokenType,
    ) -> String {
        let rule_name = self.grammar.get_rule_name(rule_id);
        let error_marker = match token_type {
            TokenType::Regular => String::new(),
            TokenType::ErrorUndefinedToken => {
                format!(" {}[UNDEFINED]{}", utils::ERROR, utils::RESET)
            }
        };
        let token_text = format!("{}\"{}\"{}", utils::BRACKET, text, utils::RESET);
        format!(
            "{}{}{}{}{}: {}",
            indent_str,
            utils::TERMINAL,
            rule_name,
            utils::RESET,
            error_marker,
            token_text
        )
    }
    pub fn pretty_forest(
        &mut self,
        trees: impl IntoIterator<Item = Arc<impl Tree + ?Sized>>,
    ) -> String {
        let mut result = String::new();
        for tree in trees {
            let tree_str = self.pretty_tree(tree);
            result.push_str(&tree_str);
            result.push('\n');
        }
        if !result.is_empty() {
            result.pop(); // remove last newline
        }
        result
    }

    fn _pretty_green_path(&mut self, path: Vec<String>, green_id: GreenId) -> String {
        // get the ReadGuard and obtain a &dyn GreenTree
        let guard = self.alloc.get_green_by_id(green_id);
        let gt: &dyn GreenTree = &**guard;

        // downcast to concrete green types via trait helpers
        if let Some(node) = gt.as_node() {
            self.pretty_node_path(path, node.id, &node.children, &node.node_type, green_id)
        } else if let Some(token) = gt.as_token() {
            self.pretty_token_path(
                path,
                token.id,
                token.text.as_str(),
                &token.token_type,
                green_id,
            )
        } else {
            String::new()
        }
    }

    fn pretty_node_path(
        &mut self,
        mut path: Vec<String>,
        rule_id: RuleId,
        children: &Vec<GreenId>,
        node_type: &NodeType,
        _green_id: GreenId,
    ) -> String {
        let rule_name = self.grammar.get_rule_name(rule_id);
        let is_error = !matches!(node_type, NodeType::Regular);
        let error_marker = if is_error {
            match node_type {
                NodeType::ErrorIncomplete => {
                    format!(" {}[INCOMPLETE]{}", utils::ERROR, utils::RESET)
                }
                NodeType::ErrorMismatch { expected } => format!(
                    " {}[ERROR: expected {}]{}",
                    utils::ERROR,
                    self.expected_names(expected),
                    utils::RESET
                ),
                NodeType::ErrorUnexpectedEOF => {
                    format!(" {}[UNEXPECTED_EOF]{}", utils::ERROR, utils::RESET)
                }
                NodeType::Regular => String::new(),
            }
        } else {
            String::new()
        };

        path.push(format!("{}{}", rule_name, error_marker));

        let mut results = Vec::new();
        for child_id in children {
            let child_result = self._pretty_green_path(path.clone(), *child_id);
            if !child_result.trim().is_empty() {
                results.push(child_result);
            }
        }

        if results.is_empty() && is_error {
            // If this is an error node with no children, show it as a leaf
            let colored_path_parts: Vec<String> = path
                .iter()
                .enumerate()
                .map(|(depth, part)| self.color_by_depth(depth, part))
                .collect();
            colored_path_parts.join(&format!("{}::{}", utils::OPERATOR, utils::RESET))
        } else if results.is_empty() {
            // If no children produced output, return empty
            String::new()
        } else {
            results.join("\n")
        }
    }

    fn pretty_token_path(
        &mut self,
        mut path: Vec<String>,
        rule_id: RuleId,
        text: &str,
        token_type: &TokenType,
        _green_id: GreenId,
    ) -> String {
        let rule_name = self.grammar.get_rule_name(rule_id);
        path.push(rule_name.to_string());

        let error_marker = match token_type {
            TokenType::Regular => String::new(),
            TokenType::ErrorUndefinedToken => {
                format!(" {}[UNDEFINED]{}", utils::ERROR, utils::RESET)
            }
        };

        let colored_path_parts: Vec<String> = path
            .iter()
            .enumerate()
            .map(|(depth, part)| self.color_by_depth(depth, part))
            .collect();

        let path_str = colored_path_parts.join(&format!("{}::{}", utils::OPERATOR, utils::RESET));
        let token_text = format!("{}\"{}\"{}", utils::BRACKET, text, utils::RESET);
        format!("{}{}: {}", path_str, error_marker, token_text)
    }

    fn _pretty_green(&mut self, indent_level: u8, green_id: GreenId) -> String {
        // get the ReadGuard and obtain a &dyn GreenTree
        let guard = self.alloc.get_green_by_id(green_id);
        let gt: &dyn GreenTree = &**guard;

        // downcast to concrete green types via trait helpers
        if let Some(node) = gt.as_node() {
            self.pretty_node(
                indent_level,
                node.id,
                &node.children,
                &node.node_type,
                green_id,
            )
        } else if let Some(token) = gt.as_token() {
            self.pretty_token(
                indent_level,
                token.id,
                token.text.as_str(),
                &token.token_type,
                green_id,
            )
        } else {
            String::new()
        }
    }
    fn pretty_node(
        &mut self,
        indent_level: u8,
        rule_id: RuleId,
        children: &Vec<GreenId>,
        node_type: &NodeType,
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
            utils::RESET
        );

        let error_marker = match node_type {
            NodeType::Regular => String::new(),
            NodeType::ErrorIncomplete => format!(" {}[INCOMPLETE]{}", utils::ERROR, utils::RESET),
            NodeType::ErrorMismatch { expected } => format!(
                " {}[MISMATCH expected: {}]{}",
                utils::ERROR,
                self.expected_names(expected),
                utils::RESET
            ),
            NodeType::ErrorUnexpectedEOF => {
                format!(" {}[UNEXPECTED_EOF]{}", utils::ERROR, utils::RESET)
            }
        };

        let mut result = format!("{}{}{}:\n", indent, rule_name, error_marker);
        for child_id in children {
            let child_str = self._pretty_green(indent_level + 1, *child_id);
            result.push_str(&child_str);
            result.push('\n');
        }
        result.pop();
        self.memo.insert((indent_level, green_id), result.clone());
        result
    }
    // changed to borrow token text
    fn pretty_token(
        &mut self,
        indent_level: u8,
        rule_id: RuleId,
        text: &str,
        token_type: &TokenType,
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
            utils::RESET
        );

        let error_marker: String = match token_type {
            TokenType::Regular => String::new(),
            TokenType::ErrorUndefinedToken => {
                format!(" {}[UNDEFINED]{}", utils::ERROR, utils::RESET)
            }
        };

        let token_text = format!("{}\"{}\"{}", utils::BRACKET, text, utils::RESET);
        let result = format!("{}{}{}: {}", indent, rule_name, error_marker, token_text);
        self.memo.insert((indent_level, green_id), result.clone());
        result
    }
}
