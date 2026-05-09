use std::fmt::Display;

use crate::stream::LineSeparator;

#[derive(Copy, Clone, Debug, Eq, Default)]
pub(crate) struct SourceChar {
    pub ch: char,
    pub index: SourceIndex,
}

impl SourceChar {
    pub fn is_number_special(&self) -> bool {
        matches!(self.ch, 'f' | 'i' | 'u' | '_' | '0'..='9')
    }

    pub fn is_alpha(&self) -> bool {
        matches!(self.ch, 'a'..='z' | 'A'..='Z' | '_')
    }

    pub fn is_number(&self) -> bool {
        self.ch.is_ascii_digit()
    }
    pub fn is_alpha_or_number(&self) -> bool {
        self.is_alpha() || self.is_number()
    }
}

impl LineSeparator for SourceChar {
    type Item = SourceChar;

    fn is_line_separator(x: &Self::Item) -> bool {
        x.ch == '\n'
    }
}

impl PartialEq for SourceChar {
    fn eq(&self, other: &Self) -> bool {
        self.ch == other.ch
    }
}

impl From<char> for SourceChar {
    fn from(value: char) -> Self {
        SourceChar {
            ch: value,
            index: SourceIndex::default(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SourceIndex {
    pub row: usize,
    pub col: usize,
}

impl SourceIndex {
    pub fn step_row(&mut self) {
        self.row += 1;
        self.col = 0;
    }

    pub fn step_col(&mut self) {
        self.col += 1;
    }
}

impl Default for SourceIndex {
    fn default() -> Self {
        Self { row: 1, col: 0 }
    }
}

impl From<(usize, usize)> for SourceIndex {
    fn from(value: (usize, usize)) -> Self {
        Self {
            row: value.0,
            col: value.1,
        }
    }
}
impl From<(i32, usize)> for SourceIndex {
    fn from(value: (i32, usize)) -> Self {
        Self {
            row: value.0 as usize,
            col: value.1,
        }
    }
}
impl From<(i32, i32)> for SourceIndex {
    fn from(value: (i32, i32)) -> Self {
        Self {
            row: value.0 as usize,
            col: value.1 as usize,
        }
    }
}

impl Display for SourceIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}:{})", self.row, self.col)
    }
}

impl PartialOrd for SourceIndex {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.row == other.row {
            Some(self.col.cmp(&other.col))
        } else {
            Some(self.row.cmp(&other.row))
        }
    }
}

impl Iterator for Vec<char> {
    type Item = SourceCharIter;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}

pub struct SourceCharIter {}
#[cfg(test)]
mod source_index_tests {

    use super::*;

    #[test]
    fn partial_ord_row() {
        let bigger = SourceIndex { row: 10, col: 20 };

        let smaller = SourceIndex { row: 5, col: 10 };

        assert!(bigger > smaller);
        assert!(smaller < bigger);
        assert_ne!(smaller, bigger);
    }

    #[test]
    fn partial_ord_same_row() {
        let bigger = SourceIndex { row: 10, col: 20 };

        let smaller = SourceIndex { row: 10, col: 10 };

        assert!(bigger > smaller);
        assert!(smaller < bigger);
        assert_ne!(smaller, bigger);
    }
    #[test]
    fn partial_ord_eq() {
        let s1 = SourceIndex { row: 10, col: 10 };
        let s2 = SourceIndex { row: 10, col: 10 };

        assert_eq!(s1, s2);
    }
}
