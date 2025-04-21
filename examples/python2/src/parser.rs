use super::lexer::{tokenize, Token};
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
    fn predicate_varargslist_1(&self) -> bool {
        matches!(self.peek(1), Token::LPar | Token::Name)
    }
    fn predicate_fplist_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_simple_stmts_1(&self) -> bool {
        self.peek(1) != Token::Newline
    }
    fn predicate_print_stmt_1(&self) -> bool {
        !matches!(self.peek(1), Token::Newline | Token::Semi)
    }
    fn predicate_import_as_names_1(&self) -> bool {
        !matches!(self.peek(1), Token::Newline | Token::Semi | Token::RPar)
    }
    fn predicate_testlist_safe_1(&self) -> bool {
        !matches!(self.peek(1), Token::For | Token::If | Token::RBrak)
    }
    fn predicate_listmaker_1(&self) -> bool {
        self.peek(1) != Token::RBrak
    }
    fn predicate_testlist_comp_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_subscriptlist_1(&self) -> bool {
        self.peek(1) != Token::RBrak
    }
    fn predicate_exprlist_1(&self) -> bool {
        !matches!(self.peek(1), Token::In | Token::Newline | Token::Semi)
    }
    fn predicate_testlist_1(&self) -> bool {
        matches!(
            self.peek(1),
            Token::BTick
                | Token::LBrace
                | Token::LBrak
                | Token::LPar
                | Token::Lambda
                | Token::Minus
                | Token::Name
                | Token::Not
                | Token::Int
                | Token::Float
                | Token::Imaginary
                | Token::Plus
                | Token::String
                | Token::Tilde
        )
    }
    fn predicate_dictorsetmaker_1(&self) -> bool {
        self.peek(1) != Token::RBrace
    }
    fn predicate_arglist_1(&self) -> bool {
        !matches!(self.peek(1), Token::RPar | Token::Star | Token::Star2)
    }
    fn predicate_arglist_2(&self) -> bool {
        !matches!(self.peek(1), Token::RPar | Token::Star2)
    }
}
