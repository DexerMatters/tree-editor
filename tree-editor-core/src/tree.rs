use std::sync::Arc;

pub trait TreeNode {
    fn rule(&self) -> &str;
    fn is_dirty(&self) -> bool;
    fn mark_dirty(&mut self);
    fn clear_dirty(&mut self);
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Node<'a> {
    pub rule: &'a str,
    pub children: Vec<Arc<Either<Node<'a>, Token<'a>>>>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Token<'a> {
    pub rule: &'a str,
    pub text: String,
}
