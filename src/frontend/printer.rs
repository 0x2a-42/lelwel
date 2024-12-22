use crate::{Cst, NodeRef, SemanticData};

use super::ast::*;

macro_rules! member {
    ($id: ident) => {
        format!(
            concat!("\x1b[90m", stringify!($id), "=\x1b[32m{:?}\x1b[0m"),
            $id
        )
    };
    ($id: literal) => {
        concat!("\x1b[90m", $id, "\x1b[0m")
    };
}
macro_rules! set {
    ($id: ident) => {
        format!(
            concat!("\x1b[90m", stringify!($id), "=\x1b[36m{}\x1b[0m"),
            $id
        )
    };
}
macro_rules! pos {
    ($id: expr) => {
        format!(concat!("\x1b[34m<{:?}>\x1b[0m"), $id)
    };
}
macro_rules! syntax {
    ($e: expr) => {
        format!("\x1b[33m[{:?}]\x1b[0m", $e)
    };
    ($name: literal, $e: expr) => {
        if let Some(syntax) = $e {
            format!("\x1b[90m{}=\x1b[32m[{:?}]\x1b[0m", $name, syntax.0)
        } else {
            "\x1b[33m[]\x1b[0m".to_string()
        }
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
                print!("│  ");
            } else {
                print!("   ");
            }
        }
    }
    fn branch<Visit: Fn(&mut Self)>(&mut self, last: bool, visit: Visit) {
        self.indent();
        if last {
            print!("└─ ");
        } else {
            print!("├─ ");
        }
        self.active.push(!last);
        visit(self);
        self.active.pop();
    }
    pub fn run(&mut self, cst: &Cst, sema: &SemanticData) {
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            println!("File {}", syntax!(file.syntax().0));
            self.branch(false, |s| {
                println!("{}", member!("start_decls"));
                let mut it = file.start_decls(cst).peekable();
                while let Some(decl) = it.next() {
                    s.branch(it.peek().is_none(), |s| s.print_start_decl(cst, decl));
                }
            });
            self.branch(false, |s| {
                println!("{}", member!("right_decls"));
                let mut it = file.right_decls(cst).peekable();
                while let Some(decl) = it.next() {
                    s.branch(it.peek().is_none(), |s| s.print_right_decl(cst, decl));
                }
            });
            self.branch(false, |s| {
                println!("{}", member!("skip_decls"));
                let mut it = file.skip_decls(cst).peekable();
                while let Some(decl) = it.next() {
                    s.branch(it.peek().is_none(), |s| s.print_skip_decl(cst, decl));
                }
            });
            self.branch(false, |s| {
                println!("{}", member!("token_decls"));
                let mut it = file.token_decls(cst).peekable();
                while let Some(decl) = it.next() {
                    s.branch(it.peek().is_none(), |s| s.print_token_decl(cst, decl));
                }
            });
            self.branch(true, |s| {
                println!("{}", member!("rule_decls"));
                let mut it = file.rule_decls(cst).peekable();
                while let Some(decl) = it.next() {
                    s.branch(it.peek().is_none(), |s| s.print_rule_decl(cst, sema, decl));
                }
            });
        }
    }
    fn print_token_decl(&mut self, cst: &Cst, decl: TokenDecl) {
        let name = decl.name(cst).map_or("", |(val, _)| val);
        let symbol = decl.symbol(cst).map_or("", |(val, _)| val);
        println!(
            "Token {} {} {} {}",
            member!(name),
            member!(symbol),
            pos!(decl.span(cst)),
            syntax!(decl.syntax().0),
        );
    }
    fn print_rule_decl(&mut self, cst: &Cst, sema: &SemanticData, decl: RuleDecl) {
        let name = decl.name(cst).map_or("", |(val, _)| val);
        println!(
            "Rule {} {} {}",
            member!(name),
            pos!(decl.span(cst)),
            syntax!(decl.syntax().0),
        );
        decl.regex(cst)
            .inspect(|r| self.branch(true, |s| s.print_regex(cst, sema, *r)));
    }
    fn print_start_decl(&mut self, cst: &Cst, decl: StartDecl) {
        let rule_name = decl.rule_name(cst).map_or("", |(val, _)| val);
        println!(
            "Start {} {} {}",
            member!(rule_name),
            pos!(decl.span(cst)),
            syntax!(decl.syntax().0),
        );
    }
    fn print_right_decl(&mut self, cst: &Cst, decl: RightDecl) {
        let mut token_names = vec![];
        decl.token_names(cst, |(val, _)| token_names.push(val));
        println!(
            "Right {} {} {}",
            member!(token_names),
            pos!(decl.span(cst)),
            syntax!(decl.syntax().0),
        );
    }
    fn print_skip_decl(&mut self, cst: &Cst, decl: SkipDecl) {
        let mut token_names = vec![];
        decl.token_names(cst, |(val, _)| token_names.push(val));
        println!(
            "Skip {} {} {}",
            member!(token_names),
            pos!(decl.span(cst)),
            syntax!(decl.syntax().0),
        );
    }
    fn print_regex(&mut self, cst: &Cst, sema: &SemanticData, regex: Regex) {
        let first = &sema
            .first_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |set| format!("{:?}", set));
        let follow = &sema
            .follow_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |set| format!("{:?}", set));
        let recovery = &sema
            .recovery_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |set| format!("{:?}", set));
        match regex {
            Regex::Alternation(alt) => {
                println!(
                    "Alternation {} {} {} {}",
                    set!(first),
                    set!(follow),
                    pos!(alt.span(cst)),
                    syntax!(alt.syntax().0),
                );
                let mut it = alt.operands(cst).peekable();
                while let Some(op) = it.next() {
                    self.branch(it.peek().is_none(), |s| s.print_regex(cst, sema, op));
                }
            }
            Regex::Concat(concat) => {
                println!(
                    "Concat {} {} {} {}",
                    set!(first),
                    set!(follow),
                    pos!(concat.span(cst)),
                    syntax!(concat.syntax().0),
                );
                let mut it = concat.operands(cst).peekable();
                while let Some(op) = it.next() {
                    self.branch(it.peek().is_none(), |s| s.print_regex(cst, sema, op));
                }
            }
            Regex::Paren(paren) => {
                println!(
                    "Paren {} {} {} {}",
                    set!(first),
                    set!(follow),
                    pos!(paren.span(cst)),
                    syntax!(paren.syntax().0),
                );
                paren
                    .inner(cst)
                    .inspect(|r| self.branch(true, |s| s.print_regex(cst, sema, *r)));
            }
            Regex::Optional(opt) => {
                println!(
                    "Optional {} {} {} {}",
                    set!(first),
                    set!(follow),
                    pos!(opt.span(cst)),
                    syntax!(opt.syntax().0),
                );
                opt.operand(cst)
                    .inspect(|r| self.branch(true, |s| s.print_regex(cst, sema, *r)));
            }
            Regex::Star(star) => {
                println!(
                    "Star {} {} {} {} {}",
                    set!(first),
                    set!(follow),
                    set!(recovery),
                    pos!(star.span(cst)),
                    syntax!(star.syntax().0),
                );
                star.operand(cst)
                    .inspect(|r| self.branch(true, |s| s.print_regex(cst, sema, *r)));
            }
            Regex::Plus(plus) => {
                println!(
                    "Plus {} {} {} {} {}",
                    set!(first),
                    set!(follow),
                    set!(recovery),
                    pos!(plus.span(cst)),
                    syntax!(plus.syntax().0),
                );
                plus.operand(cst)
                    .inspect(|r| self.branch(true, |s| s.print_regex(cst, sema, *r)));
            }
            Regex::Name(name) => {
                let value = name.value(cst).map_or("", |(val, _)| val);
                let binding = sema.decl_bindings.get(&name.syntax());
                println!(
                    "Name {} {} {} {} {} {}",
                    member!(value),
                    syntax!("binding", binding),
                    set!(first),
                    set!(follow),
                    pos!(name.span(cst)),
                    syntax!(name.syntax().0),
                );
            }
            Regex::Symbol(symbol) => {
                let value = symbol.value(cst).map_or("", |(val, _)| val);
                let binding = sema.decl_bindings.get(&symbol.syntax());
                println!(
                    "Symbol {} {} {} {} {} {}",
                    member!(value),
                    syntax!("binding", binding),
                    set!(first),
                    set!(follow),
                    pos!(symbol.span(cst)),
                    syntax!(symbol.syntax().0),
                );
            }
            Regex::Predicate(pred) => {
                let value = pred.value(cst).map_or("", |(val, _)| val);
                println!(
                    "Predicate {} {} {} {} {}",
                    member!(value),
                    set!(first),
                    set!(follow),
                    pos!(pred.span(cst)),
                    syntax!(pred.syntax().0),
                );
            }
            Regex::Action(alt) => {
                let value = alt.value(cst).map_or("", |(val, _)| val);
                println!(
                    "Action {} {} {} {} {}",
                    member!(value),
                    set!(first),
                    set!(follow),
                    pos!(alt.span(cst)),
                    syntax!(alt.syntax().0),
                );
            }
            Regex::NodeRename(rename) => {
                let value = rename.value(cst).map_or("", |(val, _)| val);
                println!(
                    "NodeRename {} {} {} {} {}",
                    member!(value),
                    set!(first),
                    set!(follow),
                    pos!(rename.span(cst)),
                    syntax!(rename.syntax().0),
                );
            }
            Regex::NodeElision(elision) => {
                println!(
                    "NodeElision {} {} {} {} {}",
                    set!(first),
                    set!(follow),
                    set!(recovery),
                    pos!(elision.span(cst)),
                    syntax!(elision.syntax().0),
                );
            }
            Regex::NodeMarker(marker) => {
                let value = marker.value(cst).map_or("", |(val, _)| val);
                println!(
                    "NodeMarker {} {} {} {} {}",
                    member!(value),
                    set!(first),
                    set!(follow),
                    pos!(marker.span(cst)),
                    syntax!(marker.syntax().0),
                );
            }
            Regex::NodeCreation(creation) => {
                let value = creation.value(cst).map_or("", |(val, _)| val);
                println!(
                    "NodeCreation {} {} {} {} {}",
                    member!(value),
                    set!(first),
                    set!(follow),
                    pos!(creation.span(cst)),
                    syntax!(creation.syntax().0),
                );
            }
        }
    }
}
