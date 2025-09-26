use std::{
    cell::Cell,
    collections::{HashMap, hash_map::DefaultHasher},
    fmt::Debug,
    hash::{Hash, Hasher},
    sync::{
        Arc, Mutex, OnceLock,
        atomic::{AtomicU64, Ordering},
    },
};

use chashmap::ReadGuard;

use crate::lang::{Grammar, RuleId};

pub type GreenId = u64;
pub type TreeId = u64;

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
        hash: OnceLock<u64>,
    },
    Token {
        id: RuleId,
        text: String,
        hash: OnceLock<u64>,
    },
    Hole {
        text: String,
        hole_type: HoleType,
        hash: OnceLock<u64>,
    },
}

impl GreenTree {
    pub fn fresh_id(&self) -> u64 {
        match self {
            GreenTree::Node { hash, .. }
            | GreenTree::Token { hash, .. }
            | GreenTree::Hole { hash, .. } => *hash.get_or_init(|| {
                let mut hasher = DefaultHasher::new();
                self.hash(&mut hasher);
                hasher.finish()
            }),
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

#[derive(Clone)]
pub enum Tree {
    Node {
        parent: Option<Arc<Tree>>,
        green: GreenId,
    },
    Token {
        parent: Option<Arc<Tree>>,
        left: OnceLock<Arc<Tree>>,
        right: OnceLock<Arc<Tree>>,
        green: GreenId,
    },
    Hole {
        parent: Option<Arc<Tree>>,
        left: OnceLock<Arc<Tree>>,
        right: OnceLock<Arc<Tree>>,
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
    pub fn left(&self) -> Option<Arc<Tree>> {
        match self {
            Tree::Token { left, .. } | Tree::Hole { left, .. } => left.get().cloned(),
            _ => None,
        }
    }
    pub fn right(&self) -> Option<Arc<Tree>> {
        match self {
            Tree::Token { right, .. } | Tree::Hole { right, .. } => right.get().cloned(),
            _ => None,
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
            hash: OnceLock::new(),
        });
        // create fresh OnceLock instances and set them if values supplied
        let left_lock: OnceLock<Arc<Tree>> = OnceLock::new();
        let right_lock: OnceLock<Arc<Tree>> = OnceLock::new();
        if let Some(l) = left {
            left_lock.set(l).unwrap();
        }
        if let Some(r) = right {
            right_lock.set(r).unwrap();
        }
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
            hash: OnceLock::new(),
        });
        let left_lock: OnceLock<Arc<Tree>> = OnceLock::new();
        let right_lock: OnceLock<Arc<Tree>> = OnceLock::new();
        if let Some(l) = left {
            left_lock.set(l).unwrap();
        }
        if let Some(r) = right {
            right_lock.set(r).unwrap();
        }
        Arc::new(Tree::Hole {
            parent,
            left: left_lock,
            right: right_lock,
            green,
        })
    }
}
