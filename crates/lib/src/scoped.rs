use std::hash::Hash;

use fxhash::FxHashMap;
use itertools::Itertools;

#[derive(Debug, Clone)]
pub struct ScopedMap<K, V> {
    scopes: Vec<FxHashMap<K, V>>,
}

impl<K: Eq + Hash, V> ScopedMap<K, V> {
    pub fn new() -> Self {
        Self {
            scopes: Vec::from([FxHashMap::default()]),
        }
    }

    pub fn push(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    pub fn pop(&mut self) {
        if self.scopes.len() <= 1 {
            panic!("Cannot pop the global scope")
        }
        self.scopes.pop();
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.scopes.last_mut().and_then(|s| s.insert(key, value));
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.scopes.iter().rev().find_map(|s| s.get(key))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.scopes
            .iter()
            .rev()
            .flat_map(|s| s.iter())
            .unique_by(|&(k, _)| k)
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.iter().map(|(k, _)| k)
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.iter().map(|(_, v)| v)
    }
}

impl<K: Eq + Hash, V> Default for ScopedMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_core() {
        let mut scopes = ScopedMap::new();
        scopes.insert("x", 1);
        scopes.insert("y", 2);

        assert_eq!(scopes.get(&"x"), Some(&1));
        assert_eq!(scopes.get(&"y"), Some(&2));

        scopes.push();
        scopes.insert("x", 42);
        scopes.insert("z", 3);

        assert_eq!(scopes.get(&"x"), Some(&42));
        assert_eq!(scopes.get(&"y"), Some(&2));
        assert_eq!(scopes.get(&"z"), Some(&3));

        scopes.pop();
        assert_eq!(scopes.get(&"x"), Some(&1));
        assert_eq!(scopes.get(&"z"), None);
    }

    #[test]
    fn test_iter() {
        let mut scopes = ScopedMap::new();
        scopes.insert("x", 1);
        scopes.insert("y", 2);

        scopes.push();
        scopes.insert("x", 42);
        scopes.insert("z", 3);

        assert_eq!(
            scopes.iter().collect::<FxHashMap<_, _>>(),
            [(&"x", &42), (&"y", &2), (&"z", &3)]
                .iter()
                .cloned()
                .collect::<FxHashMap<_, _>>()
        );
    }

    #[test]
    fn test_keys() {
        let mut scopes = ScopedMap::new();
        scopes.insert("x", 1);
        scopes.insert("y", 2);

        scopes.push();
        scopes.insert("x", 42);
        scopes.insert("z", 3);

        assert_eq!(
            scopes.keys().collect::<Vec<_>>(),
            [(&"x", &42), (&"y", &2), (&"z", &3)]
                .iter()
                .cloned()
                .collect::<FxHashMap<_, _>>()
                .keys()
                .cloned()
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_values() {
        let mut scopes = ScopedMap::new();
        scopes.insert("x", 1);
        scopes.insert("y", 2);

        scopes.push();
        scopes.insert("x", 42);
        scopes.insert("z", 3);

        assert_eq!(
            scopes.values().collect::<Vec<_>>(),
            [(&"x", &42), (&"y", &2), (&"z", &3)]
                .iter()
                .cloned()
                .collect::<FxHashMap<_, _>>()
                .values()
                .cloned()
                .collect::<Vec<_>>()
        );
    }

    #[test]
    #[should_panic(expected = "Cannot pop the global scope")]
    fn test_pop_global_panics() {
        let mut scopes = ScopedMap::<&str, i32>::new();
        scopes.pop();
    }
}
