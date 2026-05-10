use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct ScopeId(u64);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) struct TypeId(u64);

pub(crate) struct IdGenerator {
    current: u64,
}

impl IdGenerator {
    pub fn new(seed: u64) -> Self {
        Self { current: seed }
    }

    fn next(&mut self) -> u64 {
        self.current + 1
    }

    pub fn next_scope(&mut self) -> ScopeId {
        ScopeId(self.next())
    }

    pub fn next_type(&mut self) -> TypeId {
        TypeId(self.next())
    }
}

pub trait NextScopeId {
    fn next_scope_id(&mut self) -> ScopeId;
}

impl NextScopeId for IdGenerator {
    fn next_scope_id(&mut self) -> ScopeId {
        ScopeId(self.next())
    }
}

// --- old

struct ScopeTree {
    scope: ScopeId,
    children: HashMap<ScopeId, ScopeTree>,
}

impl ScopeTree {
    fn root(id: ScopeId) -> Self {
        Self {
            scope: id,
            children: HashMap::new(),
        }
    }
    fn is_parent_of(&self, _child: ScopeId, _parent: ScopeId) -> bool {
        false
    }

    fn get_parents(&self, _child: ScopeId) -> ScopeId {
        ScopeId(0)
    }

    fn find_child(&self, _needle: ScopeId) -> Option<&ScopeTree> {
        return None;
        // if self.children.contains_key(&needle) {
        //     self.children.get(&needle)
        // } else {
        //     for child in self.children.iter() {
        //         return child.1.find_child(needle);
        //     }
        //     None
        // }
    }
}
