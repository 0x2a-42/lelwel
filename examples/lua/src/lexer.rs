use crate::parser::{Diagnostic, Span};
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
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedString => Diagnostic::error()
                .with_message("unterminated string")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedComment => Diagnostic::error()
                .with_message("unterminated comment")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}

fn parse_long_string(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let prefix_len = lexer.slice().len();
    let closing = format!("]{}]", "=".repeat(prefix_len - 2));
    lexer
        .remainder()
        .find(&closing)
        .map(|i| lexer.bump(i + prefix_len))
        .ok_or_else(|| {
            lexer.bump(lexer.remainder().len());
            LexerError::UnterminatedString
        })?;
    Ok(())
}
fn parse_short_string(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let closing = lexer.slice().chars().next().unwrap();
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            c if c == closing => {
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
fn parse_comments(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let prefix_len = lexer.slice().len() - 2;
    let closing = if prefix_len >= 2 {
        format!("]{}]", "=".repeat(prefix_len - 2))
    } else {
        "\n".to_string()
    };
    if lexer
        .remainder()
        .find(&closing)
        .map(|i| lexer.bump(i + closing.len()))
        .is_some()
    {
        Ok(())
    } else {
        lexer.bump(lexer.remainder().len());
        Err(LexerError::UnterminatedComment)
    }
}
fn parse_first_line_comment(lexer: &mut Lexer<'_, Token>) -> Token {
    if lexer.span().start == 0 {
        if let Some(i) = lexer.remainder().find('\n') {
            lexer.bump(i + 1)
        }
        return Token::FirstLineComment;
    }
    Token::Hash
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[regex(r"--\[?", parse_comments)]
    #[regex(r"--\[=*\[", parse_comments)]
    Comment,
    #[regex(r"[ \t\n\f]+")]
    Whitespace,
    #[token("break")]
    BreakKw,
    #[token("return")]
    ReturnKw,
    #[token("function")]
    FunctionKw,
    #[token("end")]
    EndKw,
    #[token("goto")]
    GotoKw,
    #[token("do")]
    DoKw,
    #[token("while")]
    WhileKw,
    #[token("repeat")]
    RepeatKw,
    #[token("if")]
    IfKw,
    #[token("then")]
    ThenKw,
    #[token("until")]
    UntilKw,
    #[token("elseif")]
    ElseifKw,
    #[token("else")]
    ElseKw,
    #[token("for")]
    ForKw,
    #[token("in")]
    InKw,
    #[token("local")]
    LocalKw,
    #[token("and")]
    AndKw,
    #[token("or")]
    OrKw,
    #[token("not")]
    NotKw,
    #[token("nil")]
    NilKw,
    #[token("false")]
    FalseKw,
    #[token("true")]
    TrueKw,
    #[token(";")]
    Semi,
    #[token("::")]
    ColonColon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("...")]
    Ellipsis,
    #[token("=")]
    Equal,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("//")]
    SlashSlash,
    #[token("^")]
    Hat,
    #[token("%")]
    Percent,
    #[token("&")]
    And,
    #[token("~")]
    Tilde,
    #[token("|")]
    Pipe,
    #[token(">>")]
    GtGt,
    #[token("<<")]
    LtLt,
    #[token("..")]
    DotDot,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEqual,
    #[token("==")]
    EqualEqual,
    #[token("~=")]
    TildeEqual,
    #[token("#", parse_first_line_comment)]
    Hash,
    FirstLineComment,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[regex("[a-zA-Z_][0-9a-zA-Z_]*")]
    Name,
    #[regex("\"|'", parse_short_string)]
    #[regex(r"\[=*\[", parse_long_string)]
    LiteralString,
    #[regex(r"[0-9]+")]
    #[regex(r"0[xX][a-fA-F0-9]+")]
    #[regex(r"[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?")]
    #[regex(r"\.[0-9]+([eE][+-]?[0-9]+)?")]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+")]
    #[regex(r"0[xX][a-fA-F0-9]+\.[a-fA-F0-9]*([pP][+-]?[a-fA-F0-9]+)?")]
    #[regex(r"0[xX]\.[a-fA-F0-9]+([pP][+-]?[a-fA-F0-9]+)?")]
    #[regex(r"0[xX][a-fA-F0-9]+[pP][+-]?[a-fA-F0-9]+")]
    Numeral,
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
