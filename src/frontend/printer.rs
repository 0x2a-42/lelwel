use super::ast::*;

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
        if let Some(adr) = $id {
            format!(
                concat!("\x1b[90m", stringify!($id), "=\x1b[33m[{:p}]\x1b[0m"),
                adr
            )
        } else {
            format!(concat!(
                "\x1b[90m",
                stringify!($id),
                "=\x1b[33m[null]\x1b[0m"
            ),)
        }
    };
}
macro_rules! addr {
    ($id: ident) => {
        format!(concat!("\x1b[33m[{:p}]\x1b[0m"), $id)
    };
}
macro_rules! pos {
    ($id: ident) => {
        format!(concat!("\x1b[34m<{:?}>\x1b[0m"), $id)
    };
}

#[derive(Default)]
pub struct DebugPrinter {
    active: Vec<bool>,
}

impl<'a> DebugPrinter {
    pub fn new() -> DebugPrinter {
        Self::default()
    }
    fn indent(&self) {
        for a in self.active.iter() {
            if *a {
                print!("│  ");
            } else {
                print!("   ");
            }
        }
    }
    fn branch(&mut self, last: bool, module: &Module<'a>, regex: RegexRef) {
        self.indent();
        if last {
            print!("└─ ");
        } else {
            print!("├─ ");
        }
        self.active.push(!last);
        self.visit_regex(module, regex);
        self.active.pop();
    }

    pub fn visit(&mut self, module: &'a Module<'a>) {
        for element in module.elements.iter() {
            self.visit_element(module, element);
        }
    }

    fn visit_element(&mut self, module: &'a Module<'a>, element: &Element<'a>) {
        let span = &element.span;
        match element.kind {
            ElementKind::Start { regex, .. } => {
                println!("start: {} {}", addr!(element), pos!(span));
                self.branch(true, module, regex);
            }
            ElementKind::Rule { name, regex, .. } => {
                println!("rule: {} {} {}", member!(name), addr!(element), pos!(span));
                self.branch(true, module, regex);
            }
            ElementKind::Token { name, ty, sym } => {
                println!(
                    "token: {} {} {} {} {}",
                    member!(name),
                    member!(ty),
                    member!(sym),
                    addr!(element),
                    pos!(span)
                );
            }
            ElementKind::Action { name, num, .. } => {
                println!(
                    "action: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(span)
                );
            }
            ElementKind::Predicate { name, num, .. } => {
                println!(
                    "predicate: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(span)
                );
            }
            ElementKind::ErrorHandler { name, num, .. } => {
                println!(
                    "error handler: {} {} {} {}",
                    member!(name),
                    member!(num),
                    addr!(element),
                    pos!(span)
                );
            }
            ElementKind::Parameters { .. } => {
                println!("parameters: {}", pos!(span));
            }
            ElementKind::Invalid => {
                println!("invalid: {}", pos!(span));
            }
        }
    }

    fn visit_regex(&mut self, module: &Module<'a>, regex: RegexRef) {
        let regex = module.get_regex(regex).unwrap();
        let span = &regex.span;
        let first = &regex.first;
        let follow = &regex.follow;
        let cancel = &regex.cancel;
        match regex.kind {
            RegexKind::Id { name, elem } => {
                let elem = module.get_element(elem);
                println!(
                    "id: {} {} {} {} {} {}",
                    member!(name),
                    ptr!(elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::Concat { ref ops, error } => {
                let error = module.get_regex(error);
                println!(
                    "concat: {} {} {} {} {}",
                    ptr!(error),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                let mut count = 0;
                for op in ops {
                    count += 1;
                    self.branch(count == ops.len(), module, *op);
                }
            }
            RegexKind::Or { ref ops, error } => {
                let error = module.get_regex(error);
                println!(
                    "or: {} {} {} {} {}",
                    ptr!(error),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                let mut count = 0;
                for op in ops {
                    count += 1;
                    self.branch(count == ops.len(), module, *op);
                }
            }
            RegexKind::Star { op } => {
                println!(
                    "star: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                self.branch(true, module, op);
            }
            RegexKind::Plus { op } => {
                println!(
                    "plus: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                self.branch(true, module, op);
            }
            RegexKind::Option { op } => {
                println!(
                    "option: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                self.branch(true, module, op);
            }
            RegexKind::Paren { op } => {
                println!(
                    "paren: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
                self.branch(true, module, op);
            }
            RegexKind::Str { val, elem } => {
                let elem = module.get_element(elem);
                println!(
                    "str: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::Predicate { val, elem, .. } => {
                let elem = module.get_element(elem);
                println!(
                    "predicate: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::Action { val, elem, .. } => {
                let elem = module.get_element(elem);
                println!(
                    "action: {} {} {} {} {} {}",
                    member!(val),
                    ptr!(elem),
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::ErrorHandler { val, elem, .. } => {
                let elem = module.get_element(elem);
                println!(
                    "error: {} {} {} {} {} {} {}",
                    member!(val),
                    ptr!(elem),
                    set!(first),
                    set!(follow),
                    set!(cancel),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::Empty => {
                println!(
                    "empty: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
            RegexKind::Invalid => {
                println!(
                    "invalid: {} {} {} {}",
                    set!(first),
                    set!(follow),
                    addr!(regex),
                    pos!(span)
                );
            }
        }
    }
}
