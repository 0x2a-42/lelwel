use crate::ast::{AstNode, Exp};
use crate::lexer::{Token, tokenize};
use codespan_reporting::diagnostic::Label;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

#[derive(Default)]
pub struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>,
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl ParserCallbacks for Parser<'_> {
    fn create_tokens(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
        tokenize(source, diags)
    }
    fn create_diagnostic(&self, span: Span, message: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary((), span)])
    }

    fn create_node_expstat(&mut self, node: NodeRef, diags: &mut Vec<Diagnostic>) {
        self.cst.children(node).for_each(|c| {
            if let Some(exp) = Exp::cast(&self.cst, c) {
                match exp {
                    Exp::Callexp(_) => {}
                    _ => {
                        diags.push(
                            Diagnostic::error()
                                .with_message("unexpected expression kind")
                                .with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                                    (),
                                    self.cst.span(c),
                                )])
                                .with_notes(vec!["note: expected call expression".to_string()]),
                        );
                    }
                }
            }
        });
    }
    fn create_node_assignstat(&mut self, node: NodeRef, diags: &mut Vec<Diagnostic>) {
        self.cst.children(node).for_each(|c| {
            if let Some(exp) = Exp::cast(&self.cst, c) {
                match exp {
                    Exp::Nameexp(_) | Exp::Indexexp(_) | Exp::Fieldexp(_) => {}
                    _ => {
                        diags.push(
                            Diagnostic::error()
                                .with_message("unexpected expression kind")
                                .with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                                    (),
                                    self.cst.span(c),
                                )])
                                .with_notes(vec![
                                    "note: expected name, index, or field expression".to_string(),
                                ]),
                        );
                    }
                }
            }
        });
    }
    fn create_node_attrib(&mut self, node: NodeRef, diags: &mut Vec<Diagnostic>) {
        self.cst
            .children(node)
            .find_map(|node| self.cst.match_token(node, Token::Name))
            .inspect(|(value, span)| {
                if *value != "const" && *value != "close" {
                    diags.push(
                        Diagnostic::error()
                            .with_message(format!("unexpected attribute name: '{value}'"))
                            .with_labels(vec![codespan_reporting::diagnostic::Label::primary(
                                (),
                                span.clone(),
                            )])
                            .with_notes(vec!["note: expected 'const' or 'close'".to_string()]),
                    );
                }
            });
    }

    fn predicate_forstat_1(&self) -> bool {
        self.peek(1) == Token::Equal
    }
    fn predicate_pars_1(&self) -> bool {
        self.peek(1) != Token::Ellipsis
    }
    fn predicate_fieldlist_1(&self) -> bool {
        self.peek(1) != Token::RBrace
    }
    fn predicate_field_1(&self) -> bool {
        self.peek(1) == Token::Equal
    }
}
