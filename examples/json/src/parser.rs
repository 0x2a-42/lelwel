use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

pub type Span = core::ops::Range<usize>;
pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

macro_rules! err {
    [$span:expr, $($tk:literal),*] => {
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
            Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary((), $span.start as usize..$span.end as usize)])
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

fn parse_string(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                lexer.bump(1);
                return Ok(());
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

#[allow(clippy::upper_case_acronyms)]
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[regex("[\u{0020}\u{000A}\u{000D}\u{0009}]+")]
    Whitespace,
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
    String,
    #[regex(r"-?(0|[1-9]\d*)(\.\d+)?([eE][+-]?\d+)?")]
    Number,
    #[regex(r"[[:alpha:]][[:alnum:]]*", |_| false)]
    Error,
}

type CstIndex = usize;

#[derive(Default)]
struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>,
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
                                        span.start + i - 1..span.start + i + j + 1,
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
                                span.start + j - 1..span.start + j + 1,
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

pub fn tokenize(
    lexer: logos::Lexer<Token>,
    diags: &mut Vec<Diagnostic>,
) -> (Vec<Token>, Vec<std::ops::Range<CstIndex>>) {
    let mut tokens = vec![];
    let mut ranges = vec![];
    let source = lexer.source();

    let mut count_brace = 0;
    let mut count_brak = 0;
    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => {
                match token {
                    Token::String => {
                        check_string(&source[span.start..span.end], &span, diags);
                    }
                    Token::LBrace => count_brace += 1,
                    Token::RBrace => count_brace -= 1,
                    Token::LBrak => count_brak += 1,
                    Token::RBrak => count_brak -= 1,
                    _ => {}
                }
                if count_brace + count_brak > 256 {
                    diags.push(
                        Diagnostic::error()
                            .with_message("bracket nesting level exceeded maximum of 256")
                            .with_labels(vec![Label::primary((), span)]),
                    );
                    break;
                }
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

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl Parser<'_> {
    #[allow(clippy::ptr_arg)]
    fn build(&mut self, _rule: Rule, _node: NodeRef, _diags: &mut Vec<Diagnostic>) {}
}
