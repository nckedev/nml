#[derive(Debug)]
pub(crate) struct Identifier {
    //TODO: Fully qualified name
    pub value: String,
}

impl Identifier {
    pub fn new(value: String) -> Self {
        Self { value }
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

trait Indentifiable {
    fn identifier(&self) -> Identifier;
}

pub enum IdentifierKind {
    Name(String),
    Type(String),
}
