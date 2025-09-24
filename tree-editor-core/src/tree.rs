use std::{
    cell::Cell,
    collections::{HashMap, hash_map::DefaultHasher},
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{
    lang::{Grammar, Rule},
    utils::LFUCache,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Error {
    UnexpectedToken { expected: String, found: String },
}

#[derive(Debug, Clone)]
pub enum TreeNode<'a> {
    Node {
        rule: &'a str,
        dirty: bool,
        parent: Option<Arc<TreeNode<'a>>>,
        children: Vec<Arc<TreeNode<'a>>>,
        hash: Cell<Option<u64>>,
    },
    Token {
        rule: &'a str,
        dirty: bool,
        parent: Option<Arc<TreeNode<'a>>>,
        text: Result<String, Error>,
        hash: Cell<Option<u64>>,
    },
}

impl<'a> Hash for TreeNode<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_hash().hash(state);
    }
}

impl<'a> PartialEq for TreeNode<'a> {
    fn eq(&self, other: &Self) -> bool {
        if self.get_hash() != other.get_hash() {
            return false;
        }
        match (self, other) {
            (
                TreeNode::Node {
                    rule: r1,
                    children: c1,
                    ..
                },
                TreeNode::Node {
                    rule: r2,
                    children: c2,
                    ..
                },
            ) => r1 == r2 && c1 == c2,
            (
                TreeNode::Token {
                    rule: r1, text: t1, ..
                },
                TreeNode::Token {
                    rule: r2, text: t2, ..
                },
            ) => r1 == r2 && t1 == t2,
            _ => false,
        }
    }
}

impl<'a> Eq for TreeNode<'a> {}

impl<'a> TreeNode<'a> {
    pub fn node(
        rule: &'a str,
        children: Vec<Arc<TreeNode<'a>>>,
        parent: Option<Arc<TreeNode<'a>>>,
    ) -> Self {
        TreeNode::Node {
            rule,
            dirty: false,
            parent,
            children,
            hash: Cell::new(None),
        }
    }
    pub fn token(rule: &'a str, text: String, parent: Option<Arc<TreeNode<'a>>>) -> Self {
        TreeNode::Token {
            rule,
            dirty: false,
            parent,
            text: Ok(text),
            hash: Cell::new(None),
        }
    }

    pub fn is_node(&self) -> bool {
        matches!(self, TreeNode::Node { .. })
    }

    pub fn is_token(&self) -> bool {
        matches!(self, TreeNode::Token { .. })
    }

    pub fn mark_dirty(&mut self) {
        match self {
            TreeNode::Node { dirty, .. } | TreeNode::Token { dirty, .. } => {
                *dirty = true;
            }
        }
    }
    pub fn is_dirty(&self) -> bool {
        match self {
            TreeNode::Node { dirty, .. } | TreeNode::Token { dirty, .. } => *dirty,
        }
    }
    pub fn get_children(&self) -> Option<&Vec<Arc<TreeNode<'a>>>> {
        match self {
            TreeNode::Node { children, .. } => Some(children),
            TreeNode::Token { .. } => None,
        }
    }
    pub fn get_rule<'s>(&self, grammar: &'s Grammar<'a>) -> Option<&'s Rule<'s>> {
        match self {
            TreeNode::Node { rule, .. } | TreeNode::Token { rule, .. } => grammar.get_rule(rule),
        }
    }

    fn get_hash(&self) -> u64 {
        let (hash_cell, needs_calc) = match self {
            TreeNode::Node { hash, .. } => (hash, hash.get().is_none()),
            TreeNode::Token { hash, .. } => (hash, hash.get().is_none()),
        };

        if needs_calc {
            let mut hasher = DefaultHasher::new();
            match self {
                TreeNode::Node { rule, children, .. } => {
                    rule.hash(&mut hasher);
                    for child in children {
                        child.get_hash().hash(&mut hasher);
                    }
                }
                TreeNode::Token { rule, text, .. } => {
                    rule.hash(&mut hasher);
                    match text {
                        Ok(t) => t.hash(&mut hasher),
                        Err(e) => e.hash(&mut hasher),
                    }
                }
            }
            let new_hash = hasher.finish();
            hash_cell.set(Some(new_hash));
            new_hash
        } else {
            hash_cell.get().unwrap()
        }
    }
}

pub(crate) struct TreeCache<'a> {
    nodes: LFUCache<u64, TreeNode<'a>>,
}

impl<'a> TreeCache<'a> {
    pub fn new(capacity: usize) -> Self {
        Self {
            nodes: LFUCache::new(capacity),
        }
    }

    pub fn new_node(
        &mut self,
        rule: &'a str,
        children: Vec<Arc<TreeNode<'a>>>,
        parent: Option<Arc<TreeNode<'a>>>,
    ) -> Arc<TreeNode<'a>> {
        let node = Arc::new(TreeNode::node(rule, children, parent));
        let hash = node.get_hash();
        if self.nodes.contains_key(&hash) {
            self.nodes.get(&hash).unwrap().clone()
        } else {
            self.nodes.insert(hash, node.clone());
            node
        }
    }

    pub fn new_token(
        &mut self,
        rule: &'a str,
        text: String,
        parent: Option<Arc<TreeNode<'a>>>,
    ) -> Arc<TreeNode<'a>> {
        let node = Arc::new(TreeNode::token(rule, text, parent));
        let hash = node.get_hash();
        if self.nodes.contains_key(&hash) {
            self.nodes.get(&hash).unwrap().clone()
        } else {
            self.nodes.insert(hash, node.clone());
            node
        }
    }
}
