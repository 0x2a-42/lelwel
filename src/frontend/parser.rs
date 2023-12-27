use super::ast::*;
use super::symbols;
use super::symbols::{StringInterner, Symbol};

use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

pub type Span = core::ops::Range<usize>;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

pub struct TokenStream<'a> {
    lexer: Lexer<'a, Token<'a>>,
    uri: &'a str,
    last_doc_comment: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(lexer: Lexer<'a, Token<'a>>, uri: &'a str) -> Self {
        Self {
            lexer,
            uri,
            last_doc_comment: "",
        }
    }
    #[inline]
    pub fn next_token(&mut self) -> Result<Token<'a>, LexerError> {
        while let Some(token) = Iterator::next(&mut self.lexer) {
            match token {
                Ok(Token::_Comment(content)) => {
                    self.last_doc_comment = content;
                }
                _ => {
                    return token;
                }
            }
        }
        Ok(Token::EOF)
    }
    #[inline]
    fn span(&self) -> Span {
        self.lexer.span()
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    InvalidInteger,
    #[default]
    Invalid,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            Self::InvalidInteger => Diagnostic::error()
                .with_message("invalid integer")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}
impl From<std::num::ParseIntError> for LexerError {
    fn from(value: std::num::ParseIntError) -> Self {
        match value.kind() {
            std::num::IntErrorKind::PosOverflow => LexerError::InvalidInteger,
            _ => LexerError::Invalid,
        }
    }
}

macro_rules! check_limit {
    ($input:expr, $current:expr, $depth:expr) => {
        if $depth > 128 {
            *$current = Token::EOF;
            return Err(Diagnostic::error()
                .with_message("exceeded recursion depth limit")
                .with_labels(vec![Label::primary((), $input.span())]));
        }
    };
}

macro_rules! err {
    [$input:expr, $($tk:literal),*] => {
        {
            let expected = [$($tk),*];
            let mut msg = "invalid syntax, expected".to_string();
            if expected.len() > 1 {
                msg.push_str(" one of: ");
            } else {
                msg.push_str(": ");
            }
            let mut count = 0;
            for e in expected {
                count += 1;
                let s = format!("{}", e);
                let s = if s.starts_with('<') && s.ends_with('>') && s.len() > 2 {
                    s
                } else {
                    format!("'{}'", s)
                };
                msg.push_str(&s);
                if count < expected.len() {
                    msg.push_str(", ");
                }
            }
            Err(Diagnostic::error()
                    .with_message(msg)
                    .with_labels(vec![Label::primary((), $input.span())]))
        }
    }
}

fn parse_code<'a>(lexer: &mut Lexer<'a, Token<'a>>) -> Option<&'a str> {
    let mut count = 1;
    while count != 0 {
        lexer
            .remainder()
            .find(|c| {
                if c == '{' {
                    count += 1;
                    true
                } else if c == '}' {
                    count -= 1;
                    true
                } else {
                    false
                }
            })
            .map(|i| lexer.bump(i + 1))?;
    }
    let len = lexer.slice().len();
    Some(&lexer.slice()[1..len - 1])
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//.*\n")]
#[logos(skip r"/\*[^*]*\*+([^/*][^*]*\*+)*/")]
#[logos(error = LexerError)]
pub enum Token<'a> {
    EOF,
    #[token("token")]
    Token,
    #[token("start")]
    Start,
    #[token("parameters")]
    Pars,
    #[token(":")]
    Colon,
    #[token(";")]
    Semi,
    #[token("=")]
    Equal,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token("|")]
    Or,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[regex("[a-zA-Z][a-zA-Z_0-9]*")]
    Id(&'a str),
    #[regex("'[^']*'")]
    Str(&'a str),
    #[regex(r"\{", parse_code)]
    Code(&'a str),
    #[regex(r"\?[0-9]+", |lex| lex.slice()[1..].parse::<u64>())]
    Predicate(u64),
    #[regex(r"#[0-9]+", |lex| lex.slice()[1..].parse::<u64>())]
    Action(u64),
    #[regex(r"!", |_| u64::MAX)]
    #[regex(r"![0-9]+", |lex| lex.slice()[1..].parse::<u64>())]
    ErrorHandler(u64),
    #[regex("(///.*\n)+")]
    _Comment(&'a str),
}

include!("./generated.rs");
