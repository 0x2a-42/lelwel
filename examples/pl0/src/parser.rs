use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

pub type Span = core::ops::Range<usize>;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

pub struct TokenStream<'a> {
    lexer: Lexer<'a, Token>,
}

impl<'a> TokenStream<'a> {
    pub fn new(lexer: Lexer<'a, Token>) -> Self {
        Self { lexer }
    }
    #[inline]
    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        if let Some(token) = Iterator::next(&mut self.lexer) {
            return token;
        }
        Ok(Token::EOF)
    }
    #[inline]
    fn span(&self) -> Span {
        self.lexer.span()
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

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"\s+")]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[token("const", ignore(case))]
    Const,
    #[token("var", ignore(case))]
    Var,
    #[token("procedure", ignore(case))]
    Procedure,
    #[token("call", ignore(case))]
    Call,
    #[token("begin", ignore(case))]
    Begin,
    #[token("end", ignore(case))]
    End,
    #[token("if", ignore(case))]
    If,
    #[token("then", ignore(case))]
    Then,
    #[token("while", ignore(case))]
    While,
    #[token("do", ignore(case))]
    Do,
    #[token("odd", ignore(case))]
    Odd,
    #[token(".")]
    Dot,
    #[token("=")]
    Eq,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token(":=")]
    Asn,
    #[token("?")]
    Quest,
    #[token("!")]
    Excl,
    #[token("#")]
    Hash,
    #[token("<")]
    Lt,
    #[token("<=")]
    Leq,
    #[token(">")]
    Gt,
    #[token(">=")]
    Geq,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[regex("[[:alpha:]][[:word:]]*")]
    Ident,
    #[regex(r"\d+")]
    Number,
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));
