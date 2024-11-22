use crate::source_char::SourceIndex;

#[derive(PartialEq, Debug, Clone, Default)]
pub(crate) struct Span {
    pub start: SourceIndex,
    pub end: SourceIndex,
}

impl From<(SourceIndex, SourceIndex)> for Span {
    fn from(value: (SourceIndex, SourceIndex)) -> Self {
        Self {
            start: value.0,
            end: value.1,
        }
    }
}

trait HasSpan {
    fn span(&self) -> Span;
}
