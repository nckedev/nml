use crate::span::Span;

#[derive(Debug)]
pub(crate) struct Identifier {
    //TODO: Fully qualified name
    pub value: String,
    pub span: Span,
}

impl Identifier {
    pub fn new(value: String, span: Span) -> Self {
        Self { value, span }
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
