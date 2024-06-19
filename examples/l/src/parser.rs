use codespan_reporting::diagnostic::Label;
use logos::Logos;

pub type Span = core::ops::Range<usize>;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

macro_rules! err {
    [$span:expr, $($tk:literal),*] => {
        Diagnostic::error()
            .with_message(syntax_error_message!($span, $($tk),*))
            .with_labels(vec![Label::primary((), $span.start as usize..$span.end as usize)])
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[regex(r"[ \t\n\f]+")]
    Whitespace,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("->")]
    Arrow,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(";")]
    Semi,
    #[token("=")]
    Asn,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[regex("[a-zA-Z][a-zA-Z_0-9]*")]
    Name,
    #[regex(r"[0-9]+")]
    Int,
    Error,
}

type CstIndex = usize;

#[derive(Default)]
struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>,
}

pub fn tokenize(
    lexer: logos::Lexer<Token>,
    diags: &mut Vec<Diagnostic>,
) -> (Vec<Token>, Vec<std::ops::Range<CstIndex>>) {
    let mut tokens = vec![];
    let mut ranges = vec![];

    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => {
                tokens.push(token);
            }
            Err(err) => {
                diags.push(err.into_diagnostic(span.clone()));
                tokens.push(Token::Error);
            }
        }
        ranges.push(span.start as CstIndex..span.end as CstIndex);
    }
    (tokens, ranges)
}

impl<'a> PredicatesAndActions for Parser<'a> {
    fn predicate_param_list_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_arg_list_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));
