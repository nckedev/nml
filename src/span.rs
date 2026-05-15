use std::fmt::Display;

use crate::source_char::SourceIndex;

#[derive(PartialEq, Debug, Clone, Copy, Default)]
pub(crate) struct Span {
    pub start: SourceIndex,
    pub end: SourceIndex,
}

impl Span {
    pub fn merge(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            end: end.end,
        }
    }
}

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

#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct ByteOffset {
    pub start: usize,
    pub end: usize,
}

trait SliceFromByteOffset {
    fn slice_from_offset(&self, offset: ByteOffset) -> Self;
}

impl SliceFromByteOffset for &str {
    fn slice_from_offset(&self, offset: ByteOffset) -> Self {
        &self[offset.start..=offset.end]
    }
}

trait HasSpan {
    fn span(&self) -> Span;
}
