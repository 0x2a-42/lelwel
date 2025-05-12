use super::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use logos::Logos;

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

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("<")]
    Left,
    #[token(">")]
    Right,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,

    #[regex(r"[^+\-<>.,\[\]]+", logos::skip)]
    Ignored,

    Error,
}

pub fn tokenize(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
    let lexer = Token::lexer(source);
    let mut tokens = vec![];
    let mut spans = vec![];

    // Track open brackets
    let mut stack = vec![];

    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => {
                match token {
                    Token::LBrack => stack.push(span.clone()),
                    Token::RBrack => {
                        if stack.pop().is_none() {
                            diags.push(
                                Diagnostic::error()
                                    .with_message("unmatched closing bracket ']'")
                                    .with_labels(vec![Label::primary((), span.clone())]),
                            );
                        }
                    }
                    _ => {}
                }
                tokens.push(token);
            }
            Err(err) => {
                diags.push(err.into_diagnostic(span.clone()));
                tokens.push(Token::Error);
            }
        }
        spans.push(span);
    }

    // Report all unmatched opening brackets
    for unclosed_span in stack {
        diags.push(
            Diagnostic::error()
                .with_message("unclosed bracket '['")
                .with_labels(vec![Label::primary((), unclosed_span)]),
        );
    }

    (tokens, spans)
}
