use std::fmt::Display;

#[derive(Default, PartialEq, Eq, PartialOrd, Ord)]
enum LogLevel {
    Debug,
    #[default]
    Info,
    Warning,
    Error,
    Critical,
}

impl Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogLevel::Debug => write!(f, "DEBUG"),
            LogLevel::Info => write!(f, "INFO"),
            LogLevel::Warning => write!(f, "WARNING"),
            LogLevel::Error => write!(f, "ERROR"),
            LogLevel::Critical => write!(f, "CRITICAL"),
        }
    }
}
pub(crate) struct Log {
    level: LogLevel,
}

// TODO: get from config somehow
static LEVEL: LogLevel = LogLevel::Info;

impl Log {
    fn print(level: LogLevel, message: &str) {
        if LEVEL >= level {
            println!("{level}: {message}");
        }
    }
    pub fn debug(message: &str) {
        Log::print(LogLevel::Debug, message);
    }
    pub fn info(message: &str) {
        Log::print(LogLevel::Info, message);
    }
    pub fn warning(message: &str) {
        Log::print(LogLevel::Warning, message);
    }
    pub fn error(message: &str) {
        Log::print(LogLevel::Error, message);
    }
    pub fn critcal(message: &str) {
        Log::print(LogLevel::Critical, message);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ordering() {
        assert!(LogLevel::Error >= LogLevel::Warning);
        assert!(LogLevel::Warning < LogLevel::Error);
        assert!(LogLevel::Warning == LogLevel::Warning);
    }
}
