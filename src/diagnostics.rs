use std::fmt::Display;
use std::ops::Deref;
use std::slice::Iter;

use crate::span::Span;
use crate::token::Token;
use crate::token::TokenKind;

#[derive(Debug)]
pub struct DiagEntry {
    id: String,
    module: String,
    severity: DiagSeverity,
    span: Span,
    message: String,
}

impl DiagEntry {
    fn new(
        id: String,
        module: String,
        severity: DiagSeverity,
        span: Span,
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
            span: Span::default(),
            message: message.to_string(),
        }
    }

    pub fn empty(message: String) -> Self {
        Self {
            id: "id".to_string(),
            module: "mod".to_string(),
            severity: DiagSeverity::Err,
            span: Span::default(),
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
        span: &Span,
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
