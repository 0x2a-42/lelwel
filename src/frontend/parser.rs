use crate::frontend::lexer::{Token, tokenize};
use codespan_reporting::diagnostic::Label;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

include!("./generated.rs");

impl<'a> ParserCallbacks<'a> for Parser<'a> {
    type Diagnostic = Diagnostic;
    type Context = ();

    fn create_tokens(
        _context: &mut Self::Context,
        source: &'a str,
        diags: &mut Vec<Self::Diagnostic>,
    ) -> (Vec<Token>, Vec<Span>) {
        tokenize(source, diags)
    }
    fn create_diagnostic(&self, span: Span, message: String) -> Self::Diagnostic {
        Diagnostic::error()
            .with_message(message)
            .with_label(Label::primary((), span))
    }
    fn predicate_decl_1(&self) -> bool {
        let peek = self.peek(1);
        peek == Token::Colon || (peek == Token::Hat && self.peek(2) == Token::Colon)
    }
}
