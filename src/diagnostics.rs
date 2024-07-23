pub struct DiagEntry {
    id: String,
    module: String,
    severity: DiagSeverity,
    span: DiagSpan,
    message: String,
}

impl DiagEntry {
    pub fn new(
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
    pub fn empty(message: String) -> Self {
        Self {
            id: "id".to_string(),
            module: "mod".to_string(),
            severity: DiagSeverity::Err,
            span: DiagSpan::empty(),
            message,
        }
    }
}
struct DiagSpan {
    start: usize,
    end: usize,
}

impl DiagSpan {
    pub fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}
pub enum DiagSeverity {
    Err,
    Wrn,
}

struct DiagEntryBuilder {}
