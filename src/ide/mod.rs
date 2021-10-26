mod lookup;

use crate::frontend::{ast::*, diag::*, lexer::*, sema::*, symbol::*, token::*};
use lookup::*;
use std::collections::HashMap;

#[derive(Default)]
pub struct Server<'a> {
    asts: HashMap<String, Ast<'a, Lexer>>,
}

impl<'a> Server<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn analyze(&mut self, filename: &str, contents: String) -> Diag {
        if Symbol::allocated_bytes() > 100_000_000 {
            // deallocate string table if more than 100MB are used
            self.asts.clear();
            Symbol::reset();
        }

        let mut lexer = Lexer::new(contents, false);
        let mut diag = Diag::new(filename, 1000);
        let ast = Ast::new(&mut lexer, &mut diag);

        if let Some(root) = ast.root() {
            SemanticPass::run(root, &mut diag);
            self.asts.insert(filename.to_string(), ast);
        }

        for (range, msg) in lexer.error_iter() {
            diag.error(Code::ParserError(msg), *range);
        }
        diag
    }

    pub fn close(&mut self, filename: &str) {
        let _ = self.asts.remove(filename);
    }

    pub fn goto_def(&self, filename: &str, pos: Position) -> Option<Range> {
        let module = self.asts.get(filename)?.root()?;
        Some(LookupDefinition::find(module, pos)?.range())
    }

    pub fn references(&self, filename: &str, pos: Position, with_def: bool) -> Option<Vec<Range>> {
        let module = self.asts.get(filename)?.root()?;
        Some(LookupReferences::find(module, pos, with_def))
    }

    pub fn hover(&self, filename: &str, pos: Position) -> Option<(String, Range)> {
        let hover_ret_pars = |ret: Symbol, pars: Symbol| {
            format!(
                "---\n**Return:** `{}`  \n**Parameters:** `{}`",
                if ret.is_empty() {
                    " ".into_symbol()
                } else {
                    ret
                },
                if pars.is_empty() {
                    " ".into_symbol()
                } else {
                    pars
                },
            )
        };
        let hover_element = |element: &Element| match element.kind {
            ElementKind::Rule {
                regex, ret, pars, ..
            }
            | ElementKind::Start {
                regex, ret, pars, ..
            } => {
                let doc = if !element.attr.doc().is_empty() {
                    format!("---\n{}", element.attr.doc())
                } else {
                    "".to_string()
                };
                let doc = format!(
                    "**First:** {:?}  \n**Follow:** {:?}\n{}\n{}",
                    regex.first(),
                    regex.follow(),
                    hover_ret_pars(ret, pars),
                    doc,
                );
                Some((doc, element.range()))
            }
            _ => None,
        };
        let module = self.asts.get(filename)?.root()?;
        match LookupNode::find(module, pos)? {
            Node::Element(element) => hover_element(element),
            Node::Regex(regex) => match regex {
                Regex {
                    kind:
                        RegexKind::Id { elem, .. }
                        | RegexKind::Str { elem, .. }
                        | RegexKind::Predicate { elem, .. }
                        | RegexKind::Action { elem, .. },
                    ..
                } => {
                    let doc = if let Some(element) = elem.get() {
                        if !element.attr.doc().is_empty() {
                            format!("---\n{}", element.attr.doc())
                        } else {
                            "".to_string()
                        }
                    } else {
                        "".to_string()
                    };
                    Some((
                        format!(
                            "**First:** {:?}  \n**Follow:** {:?}\n{}",
                            regex.first(),
                            regex.follow(),
                            doc,
                        ),
                        regex.range(),
                    ))
                }
                Regex {
                    kind: RegexKind::ErrorHandler { .. },
                    ..
                } => Some((
                    format!(
                        "**Follow:** {:?}  \n**Cancel:** {:?}\n",
                        regex.follow(),
                        regex.cancel()
                    ),
                    regex.range(),
                )),
                _ => Some((
                    format!(
                        "**First:** {:?}  \n**Follow:** {:?}\n",
                        regex.first(),
                        regex.follow()
                    ),
                    regex.range(),
                )),
            },
        }
    }
}
