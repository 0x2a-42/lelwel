use crate::lexer::{Token, tokenize};
use codespan_reporting::diagnostic::Label;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

#[derive(Default)]
pub struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>,
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl Parser<'_> {
    fn lookahead(&self) -> impl Iterator<Item = Token> + use<'_> {
        self.tokens[self.pos..]
            .iter()
            .filter(|tok| !Self::is_skipped(**tok) && **tok != Token::Newline)
            .copied()
    }
}

impl<'a> ParserCallbacks for Parser<'a> {
    fn create_tokens(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
        tokenize(source, diags)
    }
    fn create_diagnostic(&self, span: Span, message: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(message)
            .with_label(Label::primary((), span))
    }
    fn predicate_table_1(&self) -> bool {
        // don't skip whitespaces
        self.tokens.get(self.pos + 1).copied() != Some(Token::LBrak)
    }
    fn predicate_array_1(&self) -> bool {
        self.lookahead().next() != Some(Token::RBrak)
    }
    fn predicate_array_2(&self) -> bool {
        let mut iter = self.lookahead();
        iter.next() != Some(Token::RBrak) && iter.next() != Some(Token::RBrak)
    }
    fn predicate_array_3(&self) -> bool {
        self.lookahead().next() == Some(Token::Comma)
    }
    fn assertion_array_table_1(&self) -> Option<Diagnostic> {
        // don't skip whitespaces
        if self.tokens.get(self.pos + 1).copied() != Some(Token::RBrak) {
            return Some(
                Diagnostic::error()
                    .with_message("invalid syntax, expected: ']]'")
                    .with_label(Label::primary((), self.span())),
            );
        }
        None
    }
}
