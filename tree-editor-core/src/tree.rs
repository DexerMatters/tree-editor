use std::{
    any::Any,
    cell::Cell,
    collections::{HashMap, hash_map::DefaultHasher},
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::{
        Arc,
        atomic::{AtomicU64, Ordering},
    },
};

use chashmap::ReadGuard;

use parking_lot::Mutex;

use crate::lang::{Grammar, RuleId};

pub type GreenId = u64;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Error {
    UnexpectedToken { expected: String, found: String },
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum HoleType {
    Incomplete,
    UndefinedToken,
    Mismatch { expected: RuleId },
}

// Concrete green structs
#[derive(Debug)]
pub struct GreenNode {
    pub id: RuleId,
    pub children: Vec<GreenId>,
    pub hash: Mutex<Option<u64>>,
}

#[derive(Debug)]
pub struct GreenToken {
    pub id: RuleId,
    pub text: String,
    pub hash: Mutex<Option<u64>>,
}

#[derive(Debug)]
pub struct GreenHole {
    pub text: String,
    pub hole_type: HoleType,
    pub hash: Mutex<Option<u64>>,
}

// Hash impls for concrete structs
impl Hash for GreenNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        for child in &self.children {
            child.hash(state);
        }
    }
}

impl Hash for GreenToken {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.text.hash(state);
    }
}

impl Hash for GreenHole {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.text.hash(state);
        self.hole_type.hash(state);
    }
}

// GreenTree trait
pub trait GreenTree: Debug + Any + Send + Sync {
    fn as_any(&self) -> &dyn Any;
    fn fresh_id(&self) -> u64;
    fn rule_id(&self) -> Option<RuleId>;
    fn text(&self) -> Option<&str>;
    fn is_hole(&self) -> bool;
    fn is_undefined_token(&self) -> bool;
    fn as_node(&self) -> Option<&GreenNode> {
        self.as_any().downcast_ref()
    }
    fn as_token(&self) -> Option<&GreenToken> {
        self.as_any().downcast_ref()
    }
    fn as_hole(&self) -> Option<&GreenHole> {
        self.as_any().downcast_ref()
    }
}

pub trait LexicalEqual {
    fn lexical_equal(&self, other: &dyn GreenTree) -> bool;
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
    fn text(&self) -> Option<&str> {
        None
    }
    fn is_hole(&self) -> bool {
        false
    }
    fn is_undefined_token(&self) -> bool {
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
    fn text(&self) -> Option<&str> {
        Some(&self.text)
    }
    fn is_hole(&self) -> bool {
        false
    }
    fn is_undefined_token(&self) -> bool {
        false
    }
}

impl LexicalEqual for GreenToken {
    fn lexical_equal(&self, other: &dyn GreenTree) -> bool {
        if let Some(other_token) = other.as_token() {
            self.id == other_token.id
        } else {
            false
        }
    }
}

impl GreenTree for GreenHole {
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
        None
    }
    fn text(&self) -> Option<&str> {
        Some(&self.text)
    }
    fn is_hole(&self) -> bool {
        true
    }
    fn is_undefined_token(&self) -> bool {
        matches!(self.hole_type, HoleType::UndefinedToken)
    }
}

impl LexicalEqual for GreenHole {
    fn lexical_equal(&self, other: &dyn GreenTree) -> bool {
        if let Some(other_hole) = other.as_hole() {
            self.hole_type == other_hole.hole_type
        } else {
            false
        }
    }
}

// Concrete tree structs
#[derive(Debug)]
pub struct Node {
    pub parent: Option<TreeRef>,
    pub green: GreenId,
}

#[derive(Debug)]
pub struct Token {
    pub parent: Option<TreeRef>,
    pub left: Mutex<Option<TreeRef>>,
    pub right: Mutex<Option<TreeRef>>,
    pub green: GreenId,
}

#[derive(Debug)]
pub struct Hole {
    pub parent: Option<TreeRef>,
    pub left: Mutex<Option<TreeRef>>,
    pub right: Mutex<Option<TreeRef>>,
    pub green: GreenId,
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
    fn as_hole(&self) -> Option<&Hole> {
        self.as_any().downcast_ref()
    }
    fn as_siblings(&self) -> Option<&dyn HasSiblings> {
        if let Some(token) = self.as_token() {
            Some(token)
        } else if let Some(hole) = self.as_hole() {
            Some(hole)
        } else {
            None
        }
    }
}

pub trait HasSiblings {
    fn left(&self) -> Option<TreeRef>;
    fn right(&self) -> Option<TreeRef>;
    fn set_left(&self, left: TreeRef);
    fn set_right(&self, right: TreeRef);
}

pub type TreeRef = Arc<dyn Tree>;

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

impl HasSiblings for Token {
    fn left(&self) -> Option<TreeRef> {
        self.left.lock().clone()
    }
    fn right(&self) -> Option<TreeRef> {
        self.right.lock().clone()
    }
    fn set_left(&self, left: TreeRef) {
        let mut g = self.left.lock();
        *g = Some(left);
    }
    fn set_right(&self, right: TreeRef) {
        let mut g = self.right.lock();
        *g = Some(right);
    }
}

impl Tree for Hole {
    fn as_any(&self) -> &dyn Any {
        self
    }
    fn green(&self) -> GreenId {
        self.green
    }
}

impl HasSiblings for Hole {
    fn left(&self) -> Option<TreeRef> {
        self.left.lock().clone()
    }
    fn right(&self) -> Option<TreeRef> {
        self.right.lock().clone()
    }
    fn set_left(&self, left: TreeRef) {
        let mut g = self.left.lock();
        *g = Some(left);
    }
    fn set_right(&self, right: TreeRef) {
        let mut g = self.right.lock();
        *g = Some(right);
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

    pub fn new_token(
        &self,
        id: RuleId,
        text: String,
        parent: Option<TreeRef>,
        left: Option<TreeRef>,
        right: Option<TreeRef>,
    ) -> TreeRef {
        let green = self.new_green(GreenToken {
            id,
            text,
            hash: Mutex::new(None),
        });
        let left_lock: Mutex<Option<TreeRef>> = Mutex::new(left);
        let right_lock: Mutex<Option<TreeRef>> = Mutex::new(right);
        Arc::new(Token {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }

    pub fn new_hole(
        &self,
        text: String,
        hole_type: HoleType,
        parent: Option<TreeRef>,
        left: Option<TreeRef>,
        right: Option<TreeRef>,
    ) -> TreeRef {
        let green = self.new_green(GreenHole {
            text,
            hole_type,
            hash: Mutex::new(None),
        });
        let left_lock: Mutex<Option<TreeRef>> = Mutex::new(left);
        let right_lock: Mutex<Option<TreeRef>> = Mutex::new(right);
        Arc::new(Hole {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }
}
