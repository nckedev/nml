use std::fmt::Display;

use crate::stream::LineSeparator;

#[derive(Copy, Clone, Debug, Eq, Default)]
pub(crate) struct SourceChar {
    pub ch: char,
    pub index: SourceIndex,
}

impl SourceChar {
    pub fn is_number_special(&self) -> bool {
        match self.ch {
            'f' | 'i' | 'u' => true,
            '_' => true,
            '0'..='9' => true,
            _ => false,
        }
    }

    pub fn is_alpha(&self) -> bool {
        match self.ch {
            'a'..='z' => true,
            'A'..='Z' => true,
            '_' => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self.ch {
            '0'..='9' => true,
            _ => false,
        }
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
            return Some(self.col.cmp(&other.col));
        } else {
            return Some(self.row.cmp(&other.row));
        }
    }
}

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
        let bigger = SourceIndex { row: 10, col: 10 };

        let smaller = SourceIndex { row: 10, col: 10 };

        assert_eq!(false, bigger > smaller);
        assert_eq!(false, smaller < bigger);
        assert_eq!(smaller, bigger);
    }
}
