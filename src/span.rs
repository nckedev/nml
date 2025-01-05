use std::fmt::Display;

use crate::source_char::SourceIndex;

#[derive(PartialEq, Debug, Clone, Default)]
pub(crate) struct Span {
    pub start: SourceIndex,
    pub end: SourceIndex,
}

impl Span {}

impl From<(SourceIndex, SourceIndex)> for Span {
    fn from(value: (SourceIndex, SourceIndex)) -> Self {
        Self {
            start: value.0,
            end: value.1,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{} - {}:{}",
            self.start.row, self.start.col, self.end.row, self.end.col
        )
    }
}

trait HasSpan {
    fn span(&self) -> Span;
}
