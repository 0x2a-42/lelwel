use crate::parser::{Diagnostic, Span};
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
    #[regex(r"\(\*[^*]*\*+([^)*][^*]*\*+)*\)")]
    Comment,
    #[regex(r"\s+")]
    Whitespace,
    #[token("CONST")]
    Const,
    #[token("VAR")]
    Var,
    #[token("PROCEDURE")]
    Procedure,
    #[token("BEGIN")]
    Begin,
    #[token("END")]
    End,
    #[token("IF")]
    If,
    #[token("THEN")]
    Then,
    #[token("WHILE")]
    While,
    #[token("DO")]
    Do,
    #[token("ELSIF")]
    Elsif,
    #[token("ELSE")]
    Else,
    #[token("TYPE")]
    Type,
    #[token("DIV")]
    DivKw,
    #[token("MOD")]
    ModKw,
    #[token("OR")]
    OrKw,
    #[token("ARRAY")]
    Array,
    #[token("RECORD")]
    Record,
    #[token("OF")]
    Of,
    #[token("MODULE")]
    Module,
    #[token("REPEAT")]
    Repeat,
    #[token("UNTIL")]
    Until,
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
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token("~")]
    Not,
    #[token("&")]
    And,
    #[token(":")]
    Colon,
    #[regex("[[:alpha:]][[:alnum:]]*")]
    Ident,
    #[regex(r"\d+")]
    Number,
    Error,
}

pub fn tokenize(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
    let lexer = Token::lexer(source);
    let mut tokens = vec![];
    let mut spans = vec![];

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
        spans.push(span);
    }
    (tokens, spans)
}
