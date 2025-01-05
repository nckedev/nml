use std::collections::HashMap;

use crate::{scope::TypeId, TypeInfo};

pub struct TypeTable {
    data: HashMap<TypeId, TypeInfo>,
}

impl TypeTable {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }
    pub fn get_by_full_name(&self, key: &str) -> Option<&TypeInfo> {
        for v in self.data.values() {
            if v.full_identifier == key {
                return Some(&v);
            }
        }
        None
    }

    pub fn contains_by_full_name(&self, key: &str) -> bool {
        match self.get_by_full_name(key) {
            Some(_) => true,
            _ => false,
        }
    }
    pub fn contains(&self, key: &TypeId) -> bool {
        match self.data.get(key) {
            Some(_) => true,
            _ => false,
        }
    }

    pub fn get(&self, id: TypeId) -> Option<&TypeInfo> {
        self.data.get(&id)
    }

    /// inserts a type with id as key,
    /// returns true if successfully inserted
    /// fase if key already exists.
    pub fn insert(&mut self, t: TypeInfo) -> bool {
        if !self.data.contains_key(&t.id) {
            self.data.insert(t.id, t);
            return true;
        }
        return false;
    }
}
