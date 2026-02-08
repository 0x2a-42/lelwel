use crate::frontend::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
    UnterminatedString,
    UnterminatedComment,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            LexerError::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedString => Diagnostic::error()
                .with_message("unterminated string literal")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedComment => Diagnostic::error()
                .with_message("unterminated comment")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}

fn parse_string(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '\'' => {
                lexer.bump(1);
                return Ok(());
            }
            '\n' => return Err(LexerError::UnterminatedString),
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
fn parse_block_comment(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    if lexer
        .remainder()
        .find("*/")
        .map(|i| lexer.bump(i + 2))
        .is_some()
    {
        Ok(())
    } else {
        lexer.bump(lexer.remainder().len());
        Err(LexerError::UnterminatedComment)
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[regex(r"//[^\n]*\n", allow_greedy = true)]
    #[regex(r"/\*", parse_block_comment)]
    Comment,
    #[regex(r"///[^\n]*\n", allow_greedy = true)]
    DocComment,
    #[regex(r"[ \t\r\n\f]+")]
    Whitespace,
    #[token("token")]
    Token,
    #[token("start")]
    Start,
    #[token("right")]
    Right,
    #[token("skip")]
    Skip,
    #[token("part")]
    Part,
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
    #[token("^")]
    Hat,
    #[token("~")]
    Tilde,
    #[token("&")]
    And,
    #[token("/")]
    Slash,
    #[regex("[a-zA-Z][a-zA-Z_0-9]*")]
    Id,
    #[regex("'", parse_string)]
    Str,
    #[regex(r"\?([0-9]+|t)")]
    Predicate,
    #[regex(r"#[0-9]+")]
    Action,
    #[regex(r"![0-9]+")]
    Assertion,
    #[regex(r"@([a-zA-Z][a-zA-Z_0-9]*)?")]
    NodeRename,
    #[regex("<[0-9]+")]
    NodeMarker,
    #[regex("[0-9]*>([a-zA-Z][a-zA-Z_0-9]*)?")]
    NodeCreation,
    Error,
}

fn check_string(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let mut it = value.char_indices();
    while let Some((_, c)) = it.next() {
        if c == '\\' {
            match it.next() {
                Some((_, '\'' | '\\')) => {}
                Some((i, _)) => {
                    diags.push(
                        Diagnostic::error()
                            .with_message("invalid escape sequence")
                            .with_labels(vec![Label::primary(
                                (),
                                span.start + i - 1..span.start + i + 1,
                            )]),
                    );
                }
                _ => unreachable!(),
            }
        }
    }
}

pub fn tokenize(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
    let lexer = Token::lexer(source);
    let mut tokens = vec![];
    let mut spans = vec![];
    let source = lexer.source();

    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => {
                if token == Token::Str {
                    check_string(&source[span.clone()], &span, diags);
                }
                tokens.push(token);
            }
            Err(LexerError::UnterminatedComment) => {
                diags.push(LexerError::UnterminatedComment.into_diagnostic(span.clone()));
                tokens.push(Token::Comment);
            }
            Err(err) => {
                diags.push(err.into_diagnostic(span.clone()));
                tokens.push(Token::Error);
            }
        }
        spans.push(span);
    }
    (tokens, spans)
}
