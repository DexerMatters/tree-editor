use regex::Regex;

use crate::{
    lang::{Grammar, RuleNode},
    tree::{TreeAlloc, TreeId},
};

#[derive(Debug, Clone)]
pub struct Validator<'a> {
    pub grammar: &'a Grammar,
    pub alloc: TreeAlloc,
    focus: TreeId,
    path: Vec<usize>, // Child index path from root to current node
}

impl<'a> Validator<'a> {
    pub fn new(alloc: TreeAlloc, grammar: &'a Grammar, focus: TreeId) -> Self {
        Self {
            alloc,
            grammar,
            focus,
            path: Vec::new(),
        }
    }

    fn trival(&self) {
        
    }
}
