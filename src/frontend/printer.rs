use super::ast::*;
use super::token::*;

macro_rules! member {
    ($id: ident) => {
        format!(
            concat!("\x1b[90m", stringify!($id), "=\x1b[32m\"{}\"\x1b[0m"),
            $id
        )
    };
}
macro_rules! set {
    ($id: ident) => {
        format!(
            concat!("\x1b[90m", stringify!($id), "=\x1b[36m{:?}\x1b[0m"),
            $id
        )
    };
}
macro_rules! ptr {
    ($id: expr) => {
        format!(
            concat!("\x1b[90m", stringify!($id), "=\x1b[33m[{:p}]\x1b[0m"),
            $id
        )
    };
}
macro_rules! addr {
    ($id: ident) => {
        format!(concat!("\x1b[33m[{:p}]\x1b[0m"), $id)
    };
}
macro_rules! pos {
    ($id: ident) => {
        format!(concat!("\x1b[34m<{}>\x1b[0m"), $id)
    };
}

#[derive(Default)]
pub struct DebugPrinter {
    active: Vec<bool>,
}

impl DebugPrinter {
    pub fn new() -> DebugPrinter {
        Self::default()
    }
    fn indent(&self) {
        for a in self.active.iter() {
            if *a {
                eprint!("│  ");
            } else {
                eprint!("   ");
            }
        }
    }
    fn branch(&mut self, last: bool, regex: &Regex) {
        self.indent();
        if last {
            eprint!("└─ ");
        } else {
            eprint!("├─ ");
        }
        self.active.push(!last);
        self.visit_regex(regex);
        self.active.pop();
    }

    pub fn visit(&mut self, module: &Module) {
        for element in module.elements.iter() {
            self.visit_element(element);
        }
    }

    fn visit_element(&mut self, element: &Element) {
        let range = element.range();
        match &element.kind {
            ElementKind::Start { regex, .. } => {
                eprintln!("start: {} {}", addr!(element), pos!(range));
                self.branch(true, regex);
            }
            ElementKind::Rule { name, regex, .. } => {
                eprintln!("rule: {} {} {}", member!(name), addr!(element), pos!(range));
                self.branch(true, regex);
            }
            ElementKind::Token { name, ty, sym } => {
                eprintln!(
                    "token: {} {} {} {} {}",
                    member!(name),
                    member!(ty),
                    member!(sym),
                    addr!(element),
                    pos!(range)
                );
            }
            ElementKind::Action { name, num, .. } => {
                eprintln!(
                    "action: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(range)
                );
            }
            ElementKind::Predicate { name, num, .. } => {
                eprintln!(
                    "predicate: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(range)
                );
            }
            ElementKind::ErrorHandler { name, num, .. } => {
                eprintln!(
                    "error handler: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(range)
                );
            }
            ElementKind::Preamble { .. } => {
                eprintln!("preamble: {}", pos!(range));
            }
            ElementKind::Parameters { .. } => {
                eprintln!("parameters: {}", pos!(range));
            }
            ElementKind::ErrorCode { .. } => {
                eprintln!("error: {}", pos!(range));
            }
            ElementKind::Language { name } => {
                eprintln!("language: {} {}", member!(name), pos!(range));
            }
            ElementKind::Limit { depth } => {
                eprintln!("limit: {} {}", member!(depth), pos!(range));
            }
            ElementKind::Invalid => {
                eprintln!("invalid: {}", pos!(range));
            }
        }
    }

    fn visit_regex(&mut self, regex: &Regex) {
        let range = regex.range();
        let first = regex.first();
        let follow = regex.follow();
        match &regex.kind {
            RegexKind::Id { name, elem, .. } => {
                eprintln!(
                    "id: {} {} {} {} {} {}",
                    member!(name),
                    ptr!(*elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::Concat { ops, error } => {
                eprintln!(
                    "concat: {} {} {} {} {}",
                    ptr!(*error),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                let mut count = 0;
                for op in ops {
                    count += 1;
                    self.branch(count == ops.len(), op);
                }
            }
            RegexKind::Or { ops, error } => {
                eprintln!(
                    "or: {} {} {} {} {}",
                    ptr!(*error),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                let mut count = 0;
                for op in ops {
                    count += 1;
                    self.branch(count == ops.len(), op);
                }
            }
            RegexKind::Star { op } => {
                eprintln!(
                    "star: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                self.branch(true, op);
            }
            RegexKind::Plus { op } => {
                eprintln!(
                    "plus: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                self.branch(true, op);
            }
            RegexKind::Option { op } => {
                eprintln!(
                    "option: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                self.branch(true, op);
            }
            RegexKind::Paren { op } => {
                eprintln!(
                    "paren: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
                self.branch(true, op);
            }
            RegexKind::Str { val, elem } => {
                eprintln!(
                    "str: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(*elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::Predicate { val, elem } => {
                eprintln!(
                    "predicate: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(*elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::Action { val, elem } => {
                eprintln!(
                    "action: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(*elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::ErrorHandler { val, elem, .. } => {
                eprintln!(
                    "error: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(*elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::Empty => {
                eprintln!(
                    "empty: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
            RegexKind::Invalid => {
                eprintln!(
                    "invalid: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(range)
                );
            }
        }
    }
}
