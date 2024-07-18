#[derive(Copy, Clone, Debug)]
pub(crate) struct SourceChar {
    pub ch: char,
    pub index: SourceIndex,
}

impl SourceChar {
    pub fn new_target(ch: char, index: SourceIndex, target: SourceIndexTarget) -> Self {
        match target {
            SourceIndexTarget::NextCol => Self {
                ch,
                index: SourceIndex {
                    row: index.row,
                    col: index.col + 1,
                },
            },
            SourceIndexTarget::NextRow => todo!(),
            SourceIndexTarget::Any => todo!(),
            SourceIndexTarget::AnyForward => todo!(),
        }
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
            index: SourceIndex { row: 0, col: 0 },
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub struct SourceIndex {
    pub row: usize,
    pub col: usize,
}

impl SourceIndex {
    pub fn next_col(&self) -> Self {
        Self {
            row: self.row,
            col: self.col + 1,
        }
    }
    pub fn emtpy() -> Self {
        Self { row: 0, col: 0 }
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

pub(crate) enum SourceIndexTarget {
    NextCol,
    NextRow,
    Any,
    AnyForward,
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
