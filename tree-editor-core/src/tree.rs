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

use crate::lang::RuleId;

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
        children: Vec<Box<GreenTree>>,
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

#[derive(Debug, Clone)]
pub enum Tree {
    Node {
        parent: Option<Arc<Tree>>,
        green: GreenId,
    },
    Token {
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
        right: Arc<Mutex<Option<Arc<Tree>>>>,
        green: GreenId,
    },
    Hole {
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
        right: Arc<Mutex<Option<Arc<Tree>>>>,
        green: GreenId,
    },
}

#[derive(Debug, Clone)]
pub struct TreeAlloc {
    greens: Arc<chashmap::CHashMap<GreenId, Arc<GreenTree>>>,
}

impl TreeAlloc {
    pub fn new() -> Self {
        Self {
            greens: Arc::new(chashmap::CHashMap::new()),
        }
    }

    pub fn new_green(&self, tree: GreenTree) -> GreenId {
        let green_id = tree.fresh_id();
        self.greens.insert(green_id, Arc::new(tree));
        green_id
    }

    pub fn new_token(
        &self,
        id: RuleId,
        text: String,
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
    ) -> Arc<Tree> {
        let green = self.new_green(GreenTree::Token {
            id,
            text,
            hash: OnceLock::new(),
        });
        Arc::new(Tree::Token {
            parent,
            left: left,
            right: Arc::new(Mutex::new(None)),
            green,
        })
    }

    pub fn new_hole(
        &self,
        text: String,
        hole_type: HoleType,
        parent: Option<Arc<Tree>>,
        left: Option<Arc<Tree>>,
    ) -> Arc<Tree> {
        let green = self.new_green(GreenTree::Hole {
            text,
            hole_type,
            hash: OnceLock::new(),
        });
        Arc::new(Tree::Hole {
            parent,
            left: left,
            right: Arc::new(Mutex::new(None)),
            green,
        })
    }
}
