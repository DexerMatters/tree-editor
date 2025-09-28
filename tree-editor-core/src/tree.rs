use std::{
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

#[derive(Debug)]
pub enum GreenTree {
    Node {
        id: RuleId,
        children: Vec<GreenId>,
        hash: Mutex<Option<u64>>,
    },
    Token {
        id: RuleId,
        text: String,
        hash: Mutex<Option<u64>>,
    },
    Hole {
        text: String,
        hole_type: HoleType,
        hash: Mutex<Option<u64>>,
    },
}

impl GreenTree {
    pub fn fresh_id(&self) -> u64 {
        match self {
            GreenTree::Node { hash, .. }
            | GreenTree::Token { hash, .. }
            | GreenTree::Hole { hash, .. } => {
                // try to read cached value
                {
                    let guard = hash.lock();
                    if let Some(v) = *guard {
                        return v;
                    }
                }
                // compute, store and return
                let mut hasher = DefaultHasher::new();
                self.hash(&mut hasher);
                let v = hasher.finish();
                let mut guard = hash.lock();
                *guard = Some(v);
                v
            }
        }
    }
    pub fn rule_id(&self) -> Option<RuleId> {
        match self {
            GreenTree::Node { id, .. } | GreenTree::Token { id, .. } => Some(*id),
            GreenTree::Hole { .. } => None,
        }
    }
    pub fn text(&self) -> Option<&str> {
        match self {
            GreenTree::Token { text, .. } | GreenTree::Hole { text, .. } => Some(text),
            _ => None,
        }
    }
    pub fn is_hole(&self) -> bool {
        matches!(self, GreenTree::Hole { .. })
    }
    pub fn is_undefined_token(&self) -> bool {
        matches!(
            self,
            GreenTree::Hole {
                hole_type: HoleType::UndefinedToken,
                ..
            }
        )
    }
    pub fn lexical_equal(&self, other: &GreenTree) -> bool {
        match (self, other) {
            (GreenTree::Token { id: id1, .. }, GreenTree::Token { id: id2, .. }) => id1 == id2,
            (GreenTree::Hole { hole_type: ht1, .. }, GreenTree::Hole { hole_type: ht2, .. }) => {
                ht1 == ht2
            }
            _ => false,
        }
    }
}

impl Hash for GreenTree {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            GreenTree::Node { id, children, .. } => {
                id.hash(state);
                for child in children {
                    child.hash(state);
                }
            }
            GreenTree::Token { id, text, .. } => {
                id.hash(state);
                text.hash(state);
            }
            GreenTree::Hole {
                text, hole_type, ..
            } => {
                text.hash(state);
                hole_type.hash(state);
            }
        }
    }
}

pub enum Tree {
    Node {
        parent: Option<Arc<Tree>>,
        green: GreenId,
    },
    Token {
        parent: Option<Arc<Tree>>,
        left: Mutex<Option<Arc<Tree>>>,
        right: Mutex<Option<Arc<Tree>>>,
        green: GreenId,
    },
    Hole {
        parent: Option<Arc<Tree>>,
        left: Mutex<Option<Arc<Tree>>>,
        right: Mutex<Option<Arc<Tree>>>,
        green: GreenId,
    },
}

impl Tree {
    pub fn green(&self) -> GreenId {
        match self {
            Tree::Node { green, .. } | Tree::Token { green, .. } | Tree::Hole { green, .. } => {
                *green
            }
        }
    }
    pub fn intern_green<'a>(
        &self,
        alloc: &'a TreeAlloc,
    ) -> Option<ReadGuard<'a, GreenId, GreenTree>> {
        alloc.get_green_by_id(self.green())
    }
    pub fn left(&self) -> Option<Arc<Tree>> {
        match self {
            Tree::Token { left, .. } | Tree::Hole { left, .. } => left.lock().clone(),
            _ => None,
        }
    }
    pub fn right(&self) -> Option<Arc<Tree>> {
        match self {
            Tree::Token { right, .. } | Tree::Hole { right, .. } => right.lock().clone(),
            _ => None,
        }
    }
    pub fn set_left(&self, left: Arc<Tree>) -> () {
        if let Tree::Token {
            left: left_lock, ..
        }
        | Tree::Hole {
            left: left_lock, ..
        } = self
        {
            let mut g = left_lock.lock();
            *g = Some(left);
        }
    }
    pub fn set_right(&self, right: Arc<Tree>) -> () {
        if let Tree::Token {
            right: right_lock, ..
        }
        | Tree::Hole {
            right: right_lock, ..
        } = self
        {
            let mut g = right_lock.lock();
            *g = Some(right);
        }
    }
}

impl Debug for Tree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Tree::Node { parent, green } => f
                .debug_struct("Node")
                .field("parent", &parent.as_ref().map(|p| p))
                .field("green", green)
                .finish(),
            Tree::Token { parent, green, .. } => f
                .debug_struct("Token")
                .field("parent", &parent.as_ref().map(|p| p))
                .field("green", green)
                .finish(),
            Tree::Hole { parent, green, .. } => f
                .debug_struct("Hole")
                .field("parent", &parent.as_ref().map(|p| p))
                .field("green", green)
                .finish(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TreeAlloc {
    greens: Arc<chashmap::CHashMap<GreenId, GreenTree>>,
}

impl TreeAlloc {
    pub fn new() -> Self {
        Self {
            greens: Arc::new(chashmap::CHashMap::new()),
        }
    }

    pub fn get_green(&self, tree: impl AsRef<Tree>) -> Option<ReadGuard<'_, GreenId, GreenTree>> {
        self.greens.get(&tree.as_ref().green())
    }

    pub fn get_green_by_id(&self, green_id: GreenId) -> Option<ReadGuard<'_, GreenId, GreenTree>> {
        self.greens.get(&green_id)
    }

    pub fn new_green(&self, tree: GreenTree) -> GreenId {
        let green_id = tree.fresh_id();
        self.greens.insert(green_id, tree);
        green_id
    }

    pub fn new_token(
        &self,
        id: RuleId,
        text: String,
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
        right: Option<Arc<Tree>>,
    ) -> Arc<Tree> {
        let green = self.new_green(GreenTree::Token {
            id,
            text,
            hash: Mutex::new(None),
        });
        // create fresh Mutex<Option<Arc<Tree>>> instances and set them with provided values
        let left_lock: Mutex<Option<Arc<Tree>>> = Mutex::new(left);
        let right_lock: Mutex<Option<Arc<Tree>>> = Mutex::new(right);
        Arc::new(Tree::Token {
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
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
        right: Option<Arc<Tree>>,
    ) -> Arc<Tree> {
        let green = self.new_green(GreenTree::Hole {
            text,
            hole_type,
            hash: Mutex::new(None),
        });
        let left_lock: Mutex<Option<Arc<Tree>>> = Mutex::new(left);
        let right_lock: Mutex<Option<Arc<Tree>>> = Mutex::new(right);
        Arc::new(Tree::Hole {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }
}
