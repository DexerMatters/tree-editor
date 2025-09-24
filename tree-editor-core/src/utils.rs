use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;
use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Eq, PartialEq, Clone)]
struct CacheEntry<K> {
    key: K,
    count: usize,
    seq: usize,
}

impl<K: Ord> Ord for CacheEntry<K> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.count
            .cmp(&other.count)
            .then_with(|| self.seq.cmp(&other.seq)) // Evict older items first on tie
            .then_with(|| self.key.cmp(&other.key))
    }
}

impl<K: Ord> PartialOrd for CacheEntry<K> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct LFUCache<K, V>
where
    K: Clone + Eq + Hash + Ord,
{
    map: HashMap<K, (Arc<V>, CacheEntry<K>)>,
    order: BTreeSet<CacheEntry<K>>,
    capacity: usize,
    seq: AtomicUsize,
}

impl<K, V> LFUCache<K, V>
where
    K: Clone + Eq + Hash + Ord,
{
    pub fn new(capacity: usize) -> Self {
        if capacity == 0 {
            panic!("Capacity must be greater than 0");
        }
        Self {
            map: HashMap::with_capacity(capacity),
            order: BTreeSet::new(),
            capacity,
            seq: AtomicUsize::new(0),
        }
    }

    pub fn insert(&mut self, key: K, value: Arc<V>) {
        // Remove existing entry to update it.
        if let Some((_, old_entry)) = self.map.remove(&key) {
            self.order.remove(&old_entry);
        }

        if self.map.len() >= self.capacity {
            // Evict the entry with the lowest count and oldest sequence number.
            // BTreeSet::pop_first is efficient (logarithmic).
            if let Some(evict_entry) = self.order.pop_first() {
                self.map.remove(&evict_entry.key);
            }
        }

        let seq = self.seq.fetch_add(1, Ordering::Relaxed);
        let count = Arc::strong_count(&value);
        let entry = CacheEntry {
            key: key.clone(),
            count,
            seq,
        };

        self.map.insert(key, (value, entry.clone()));
        self.order.insert(entry);
    }

    pub fn get(&mut self, key: &K) -> Option<Arc<V>> {
        if let Some((val, entry)) = self.map.get_mut(key) {
            let new_count = Arc::strong_count(val);
            if new_count != entry.count {
                let old_entry = entry.clone();
                entry.count = new_count;
                if self.order.remove(&old_entry) {
                    self.order.insert(entry.clone());
                }
            }
            Some(val.clone())
        } else {
            None
        }
    }

    pub fn contains_key(&self, key: &K) -> bool {
        self.map.contains_key(key)
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
}
