use std::fmt::Display;
use std::ops::Deref;
use std::slice::Iter;

use crate::token::Token;
use crate::token::TokenKind;
use crate::token::TokenSpan;

#[derive(Debug)]
pub struct DiagEntry {
    id: String,
    module: String,
    severity: DiagSeverity,
    span: DiagSpan,
    message: String,
}

impl DiagEntry {
    fn new(
        id: String,
        module: String,
        severity: DiagSeverity,
        span: DiagSpan,
        message: String,
    ) -> Self {
        Self {
            id,
            module,
            severity,
            span,
            message,
        }
    }
    pub fn message_only(message: String) -> Self {
        Self {
            id: "id".to_string(),
            module: "mod".to_string(),
            severity: DiagSeverity::Err,
            span: DiagSpan::empty(),
            message: message.to_string(),
        }
    }

    pub fn empty(message: String) -> Self {
        Self {
            id: "id".to_string(),
            module: "mod".to_string(),
            severity: DiagSeverity::Err,
            span: DiagSpan::empty(),
            message,
        }
    }

    pub fn expected(found: &Token, expected: String) {}
}

impl Display for DiagEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "---")?;
        writeln!(f, "{}", self.message)?;
        writeln!(f, "---")?;
        Ok(())
    }
}
#[derive(Debug)]
struct DiagSpan {
    start: usize,
    end: usize,
}

impl DiagSpan {
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

impl From<TokenSpan> for DiagSpan {
    fn from(value: TokenSpan) -> Self {
        todo!()
    }
}
#[derive(Debug)]
pub enum DiagSeverity {
    Err,
    Wrn,
}

struct DiagEntryBuilder {}

pub struct Diagnostics {
    list: Vec<DiagEntry>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self { list: vec![] }
    }

    pub fn reset(&mut self) {
        self.list.clear()
    }

    pub fn push(&mut self, entry: DiagEntry) {
        self.list.push(entry)
    }

    pub fn push_expected_token_missmatch(
        &mut self,
        actual: &TokenKind,
        expected: String,
        span: &TokenSpan,
    ) {
        let entry = DiagEntry::message_only(format!(
            "@ expected {}, found {}",
            expected.as_str(),
            actual
        ));

        self.list.push(entry);
    }

    pub fn iter(&self) -> Iter<DiagEntry> {
        self.list.iter()
    }
}

impl IntoIterator for Diagnostics {
    type Item = DiagEntry;

    type IntoIter = <Vec<DiagEntry> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.list.into_iter()
    }
}

impl Deref for Diagnostics {
    type Target = [DiagEntry];

    fn deref(&self) -> &Self::Target {
        &self.list[..]
    }
}
