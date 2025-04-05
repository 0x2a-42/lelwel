use crate::lexer::{Token, tokenize};
use codespan_reporting::diagnostic::Label;

// TODO: change definition and all uses if codespan_reporting is not used
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

// TODO: add context information to the parser if required
#[derive(Default)]
pub struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

