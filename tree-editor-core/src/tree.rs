use std::{
    any::Any,
    collections::hash_map::DefaultHasher,
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::Arc,
};

use chashmap::ReadGuard;

use parking_lot::Mutex;

use crate::lang::RuleId;

pub type GreenId = u64;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Error {
    UnexpectedToken { expected: String, found: String },
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum NodeType {
    Regular,
    ErrorIncomplete,
    ErrorMismatch { expected: Vec<RuleId> },
    ErrorUnexpectedEOF,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TokenType {
    Regular,
    ErrorUndefinedToken,
}

// Concrete green structs
#[derive(Debug)]
pub struct GreenNode {
    pub id: RuleId,
    pub children: Vec<GreenId>,
    pub hash: Mutex<Option<u64>>,
    pub node_type: NodeType,
}

#[derive(Debug)]
pub struct GreenToken {
    pub id: RuleId,
    pub text: String,
    pub hash: Mutex<Option<u64>>,
    pub token_type: TokenType,
}

impl GreenNode {
    pub fn new(id: RuleId, children: Vec<GreenId>) -> Self {
        Self {
            id,
            children,
            hash: Mutex::new(None),
            node_type: NodeType::Regular,
        }
    }

    pub fn new_with_type(id: RuleId, children: Vec<GreenId>, node_type: NodeType) -> Self {
        Self {
            id,
            children,
            hash: Mutex::new(None),
            node_type,
        }
    }
}

// Hash impls for concrete structs
impl Hash for GreenNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        for child in &self.children {
            child.hash(state);
        }
        self.node_type.hash(state);
    }
}

impl Hash for GreenToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.text.hash(state);
        self.token_type.hash(state);
    }
}

// GreenTree trait
pub trait GreenTree: Debug + Any + Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn fresh_id(&self) -> u64;
    fn rule_id(&self) -> Option<RuleId>;
    fn is_error_node(&self) -> bool;
    fn is_error_token(&self) -> bool;
    fn as_node(&self) -> Option<&GreenNode> {
        self.as_any().downcast_ref()
    }
    fn as_token(&self) -> Option<&GreenToken> {
        self.as_any().downcast_ref()
    }
}

pub trait GreenLeaf: GreenTree {
    fn lexical_equal(&self, other: &dyn GreenTree) -> bool;
    fn text(&self) -> &str;
}

// Implement GreenTree for each concrete struct
impl GreenTree for GreenNode {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn fresh_id(&self) -> u64 {
        {
            let guard = self.hash.lock();
            if let Some(v) = *guard {
                return v;
            }
        }
        let mut hasher = DefaultHasher::new();
        Hash::hash(self, &mut hasher);
        let v = hasher.finish();
        let mut guard = self.hash.lock();
        *guard = Some(v);
        v
    }
    fn rule_id(&self) -> Option<RuleId> {
        Some(self.id)
    }
    fn is_error_node(&self) -> bool {
        !matches!(self.node_type, NodeType::Regular)
    }
    fn is_error_token(&self) -> bool {
        false
    }
}

impl GreenTree for GreenToken {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn fresh_id(&self) -> u64 {
        {
            let guard = self.hash.lock();
            if let Some(v) = *guard {
                return v;
            }
        }
        let mut hasher = DefaultHasher::new();
        Hash::hash(self, &mut hasher);
        let v = hasher.finish();
        let mut guard = self.hash.lock();
        *guard = Some(v);
        v
    }
    fn rule_id(&self) -> Option<RuleId> {
        Some(self.id)
    }
    fn is_error_node(&self) -> bool {
        false
    }
    fn is_error_token(&self) -> bool {
        !matches!(self.token_type, TokenType::Regular)
    }
}

impl GreenLeaf for GreenToken {
    fn lexical_equal(&self, other: &dyn GreenTree) -> bool {
        if let Some(other_token) = other.as_token() {
            self.id == other_token.id
        } else {
            false
        }
    }
    fn text(&self) -> &str {
        &self.text
    }
}

// Concrete tree structs
#[derive(Debug)]
pub struct Node {
    pub parent: Option<NodeRef>,
    pub green: GreenId,
}

#[derive(Debug)]
pub struct Token {
    pub parent: Option<NodeRef>,
    pub left: Mutex<Option<TokenRef>>,
    pub right: Mutex<Option<TokenRef>>,
    pub green: GreenId,
}

impl Token {
    pub fn left(&self) -> Option<TokenRef> {
        self.left.lock().clone()
    }
    pub fn right(&self) -> Option<TokenRef> {
        self.right.lock().clone()
    }
    pub fn set_left(&self, left: TokenRef) {
        let mut g = self.left.lock();
        *g = Some(left);
    }
    pub fn set_right(&self, right: TokenRef) {
        let mut g = self.right.lock();
        *g = Some(right);
    }
}

// Tree trait
pub trait Tree: Debug + Any + Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn green(&self) -> GreenId;
    fn intern_green<'a>(&self, alloc: &'a TreeAlloc) -> ReadGuard<'a, GreenId, Box<dyn GreenTree>> {
        alloc.get_green_by_id(self.green())
    }
    fn as_node(&self) -> Option<&Node> {
        self.as_any().downcast_ref()
    }
    fn as_token(&self) -> Option<&Token> {
        self.as_any().downcast_ref()
    }
}

pub type TreeRef = Arc<dyn Tree>;
pub type TokenRef = Arc<Token>;
pub type NodeRef = Arc<Node>;

// Implement Tree for each concrete struct
impl Tree for Node {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn green(&self) -> GreenId {
        self.green
    }
}

impl Tree for Token {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn green(&self) -> GreenId {
        self.green
    }
}

#[derive(Debug, Clone)]
pub struct TreeAlloc {
    pub greens: Arc<chashmap::CHashMap<GreenId, Box<dyn GreenTree>>>,
}

impl TreeAlloc {
    pub fn new() -> Self {
        Self {
            greens: Arc::new(chashmap::CHashMap::new()),
        }
    }

    pub fn get_green(&self, tree: &dyn Tree) -> Option<ReadGuard<'_, GreenId, Box<dyn GreenTree>>> {
        self.greens.get(&tree.green())
    }

    pub fn get_green_by_id(&self, green_id: GreenId) -> ReadGuard<'_, GreenId, Box<dyn GreenTree>> {
        self.greens.get(&green_id).unwrap()
    }

    pub fn new_green(&self, tree: impl GreenTree + 'static) -> GreenId {
        let green_id = tree.fresh_id();
        self.greens.insert(green_id, Box::new(tree));
        green_id
    }

    pub fn is_error_green(&self, green_id: GreenId) -> bool {
        if let Some(guard) = self.greens.get(&green_id) {
            guard.is_error_node() || guard.is_error_token()
        } else {
            false
        }
    }

    pub fn new_node(
        &self,
        id: RuleId,
        children: Vec<GreenId>,
        parent: Option<NodeRef>,
    ) -> Arc<Node> {
        let green = self.new_green(GreenNode {
            id,
            children,
            hash: Mutex::new(None),
            node_type: NodeType::Regular,
        });
        Arc::new(Node { parent, green })
    }

    pub fn new_error_node(
        &self,
        id: RuleId,
        children: Vec<GreenId>,
        node_type: NodeType,
        parent: Option<NodeRef>,
    ) -> Arc<Node> {
        let green = self.new_green(GreenNode {
            id,
            children,
            hash: Mutex::new(None),
            node_type,
        });
        Arc::new(Node { parent, green })
    }

    pub fn new_token(
        &self,
        id: RuleId,
        text: String,
        parent: Option<NodeRef>,
        left: Option<TokenRef>,
        right: Option<TokenRef>,
    ) -> Arc<Token> {
        let green = self.new_green(GreenToken {
            id,
            text,
            hash: Mutex::new(None),
            token_type: TokenType::Regular,
        });
        let left_lock = Mutex::new(left);
        let right_lock = Mutex::new(right);
        Arc::new(Token {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }

    pub fn new_error_token(
        &self,
        id: RuleId,
        text: String,
        token_type: TokenType,
        parent: Option<NodeRef>,
        left: Option<TokenRef>,
        right: Option<TokenRef>,
    ) -> Arc<Token> {
        let green = self.new_green(GreenToken {
            id,
            text,
            hash: Mutex::new(None),
            token_type,
        });
        let left_lock = Mutex::new(left);
        let right_lock = Mutex::new(right);
        Arc::new(Token {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }
}
