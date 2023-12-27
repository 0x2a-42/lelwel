use super::Value;
use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};
use std::collections::BTreeMap;

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
    UnterminatedString,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            Self::UnterminatedString => Diagnostic::error()
                .with_message("unterminated string")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}

fn parse_string<'a>(lexer: &mut Lexer<'a, Token>) -> Result<String, LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                lexer.bump(1);
                let len = lexer.slice().len();
                return Ok(lexer.slice()[1..len - 1].to_string());
            }
            '\\' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedString)
}

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip "[\u{0020}\u{000A}\u{000D}\u{0009}]+")]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("null")]
    Null,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[regex("\"", parse_string)]
    String(String),
    #[regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?", |lex| lex.slice().to_string())]
    Number(String),
    #[regex(r"[[:alpha:]][[:alnum:]]*", |_| Err(LexerError::Invalid))]
    Invalid,
}

fn check_string(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let mut it = value.chars().enumerate();
    while let Some((i, c)) = it.next() {
        match c {
            '\\' => match it.next() {
                Some((_, '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't')) => {}
                Some((i, 'u')) => {
                    for j in 0..4 {
                        if !it
                            .next()
                            .map(|(_, c)| c.is_ascii_hexdigit())
                            .unwrap_or(false)
                        {
                            diags.push(
                                Diagnostic::error()
                                    .with_message("invalid unicode escape sequence")
                                    .with_labels(vec![Label::primary(
                                        (),
                                        span.start + i..span.start + i + j + 3,
                                    )]),
                            );
                            break;
                        }
                    }
                }
                Some((j, _)) => {
                    diags.push(
                        Diagnostic::error()
                            .with_message("invalid escape sequence")
                            .with_labels(vec![Label::primary(
                                (),
                                span.start + j..span.start + j + 2,
                            )]),
                    );
                }
                _ => unreachable!(),
            },
            '\u{0020}'..='\u{10FFFF}' => {}
            c => {
                diags.push(
                    Diagnostic::error()
                        .with_message(format!("string contains invalid character {:?}", c))
                        .with_labels(vec![Label::primary((), span.start + i..span.start + i + 1)
                            .with_message("after this character")]),
                );
            }
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));
