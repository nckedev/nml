pub(crate) struct ScopeGenerator {
    current: u64,
}

impl ScopeGenerator {
    pub fn new() -> ScopeGenerator {
        Self { current: 0 }
    }

    pub fn next(&mut self) -> Scope {
        self.current += 1;
        Scope {
            id: self.current,
            parent_id: None,
        }
    }

    pub fn next_with_parent(&mut self, scope: Scope) -> Scope {
        self.current += 1;
        Scope {
            id: self.current,
            parent_id: Some(scope.id),
        }
    }
}

#[derive(Debug)]
pub(crate) struct Scope {
    id: u64,
    parent_id: Option<u64>,
}

impl Scope {
    fn new() -> Self {
        Self {
            id: 0,
            parent_id: Some(0),
        }
    }

    fn new_child(&self) -> Self {
        Self {
            id: self.id + 1,
            parent_id: self.parent_id,
        }
    }

    fn has_parent(&self) -> bool {
        match self.parent_id {
            Some(_) => true,
            None => false,
        }
    }
}
