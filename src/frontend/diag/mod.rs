mod imp;

use super::token::Range;
pub use imp::Code;
use std::fmt;

macro_rules! red {
    ($e: expr) => {
        format!("\x1b[31;1m{}\x1b[0m", $e)
    };
}
macro_rules! yellow {
    ($e: expr) => {
        format!("\x1b[33;1m{}\x1b[0m", $e)
    };
}

#[derive(Debug)]
pub struct Message {
    code: Code,
    range: Range,
    related: Vec<(Range, String)>,
}

#[derive(Debug)]
pub struct Diag {
    filename: String,
    errors: Vec<Message>,
    warnings: Vec<Message>,
    notes: Vec<Message>,
    max_count: usize,
}

impl Message {
    #[allow(dead_code)]
    fn new(code: Code, range: Range) -> Message {
        Message {
            code,
            range,
            related: vec![],
        }
    }
    #[allow(dead_code)]
    fn new_with_related(code: Code, range: Range, related: Vec<(Range, String)>) -> Message {
        Message {
            code,
            range,
            related,
        }
    }
    #[allow(dead_code)]
    pub fn code(&self) -> &Code {
        &self.code
    }
    #[allow(dead_code)]
    pub fn range(&self) -> Range {
        self.range
    }
    #[allow(dead_code)]
    pub fn related(&self) -> &[(Range, String)] {
        &self.related
    }
}

impl fmt::Display for Message {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.range, self.code)
    }
}

impl Diag {
    pub fn new(filename: &str, max_count: usize) -> Diag {
        Diag {
            filename: filename.to_string(),
            errors: vec![],
            warnings: vec![],
            notes: vec![],
            max_count,
        }
    }
    #[allow(dead_code)]
    pub fn error(&mut self, code: Code, range: Range) {
        if self.errors.len() < self.max_count {
            self.errors.push(Message::new(code, range));
        }
    }
    #[allow(dead_code)]
    pub fn error_with_related(&mut self, code: Code, range: Range, related: Vec<(Range, String)>) {
        if self.errors.len() < self.max_count {
            self.errors
                .push(Message::new_with_related(code, range, related));
        }
    }
    #[allow(dead_code)]
    pub fn warning(&mut self, code: Code, range: Range) {
        if self.warnings.len() < self.max_count {
            self.warnings.push(Message::new(code, range));
        }
    }
    #[allow(dead_code)]
    pub fn notes(&mut self, code: Code, range: Range) {
        if self.notes.len() < self.max_count {
            self.notes.push(Message::new(code, range));
        }
    }
    #[allow(dead_code)]
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    #[allow(dead_code)]
    pub fn error_iter(&self) -> std::slice::Iter<Message> {
        self.errors.iter()
    }
    #[allow(dead_code)]
    pub fn warning_iter(&self) -> std::slice::Iter<Message> {
        self.warnings.iter()
    }
    #[allow(dead_code)]
    pub fn print(&self) {
        for e in self.errors.iter() {
            eprintln!("{} {}:{}", red!("error:"), self.filename, e);
            for (r, msg) in e.related().iter() {
                eprintln!("     | {}: {}", r, msg);
            }
        }
        for e in self.warnings.iter() {
            eprintln!("{} {}:{}", yellow!("warning:"), self.filename, e);
        }
    }
    #[allow(dead_code)]
    pub fn fatal_error(msg: &str) {
        eprintln!("{} {}", red!("fatal error:"), msg);
    }
}
