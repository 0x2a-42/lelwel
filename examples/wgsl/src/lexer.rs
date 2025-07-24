use super::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
    UnterminatedComment,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_label(Label::primary((), span)),
            LexerError::UnterminatedComment => Diagnostic::error()
                .with_message("unterminated comment")
                .with_label(Label::primary((), span)),
        }
    }
}

fn lex_line_comment(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    for c in lexer.remainder().chars() {
        match c {
            '\n' => {
                lexer.bump(1);
                return Ok(());
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Ok(())
}
fn lex_block_comment(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let mut it = lexer.remainder().chars();
    let mut nesting = 0usize;
    while let Some(c) = it.next() {
        match c {
            '*' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                    if c == '/' {
                        if nesting == 0 {
                            return Ok(());
                        }
                        nesting = nesting.saturating_sub(1);
                    }
                }
            }
            '/' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                    if c == '*' {
                        nesting += 1;
                    }
                }
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedComment)
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[token("enable")]
    Enable,
    #[token("requires")]
    Requires,
    #[token("fn")]
    Fn,
    #[token("alias")]
    Alias,
    #[token("struct")]
    Struct,
    #[token("var")]
    Var,
    #[token("const_assert")]
    ConstAssert,
    #[token("if")]
    If,
    #[token("for")]
    For,
    #[token("else")]
    Else,
    #[token("loop")]
    Loop,
    #[token("break")]
    Break,
    #[token("while")]
    While,
    #[token("return")]
    Return,
    #[token("switch")]
    Switch,
    #[token("discard")]
    Discard,
    #[token("continuing")]
    Continuing,
    #[token("const")]
    Const,
    #[token("case")]
    Case,
    #[token("default")]
    Default,
    #[token("override")]
    Override,
    #[token("continue")]
    Continue,
    #[token("let")]
    Let,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("diagnostic")]
    Diagnostic,
    #[token(";")]
    Semi,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token(",")]
    Comma,
    #[token("=")]
    Eq,
    #[token(":")]
    Colon,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("->")]
    MinusGt,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token(".")]
    Dot,
    #[token("@")]
    At,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token("&")]
    And,
    #[token("!")]
    Excl,
    #[token("*")]
    Star,
    #[token("-")]
    Minus,
    #[token("~")]
    Tilde,
    #[token("+")]
    Plus,
    #[token("==")]
    Eq2,
    #[token("|")]
    Pipe,
    #[token("&&")]
    And2,
    #[token("/")]
    Slash,
    #[token("^")]
    Caret,
    #[token("||")]
    Pipe2,
    #[token("!=")]
    ExclEq,
    #[token("%")]
    Percent,
    #[token("_")]
    Underscore,
    #[token("&=")]
    AndEq,
    #[token("*=")]
    StarEq,
    #[token("+=")]
    PlusEq,
    #[token("|=")]
    PipeEq,
    #[token("-=")]
    MinusEq,
    #[token("/=")]
    SlashEq,
    #[token("^=")]
    CaretEq,
    #[token("%=")]
    PercentEq,
    #[token("++")]
    Plus2,
    #[token("--")]
    Minus2,
    #[regex(r"([_\p{XID_Start}][\p{XID_Continue}]+)|[\p{XID_Start}]")]
    Ident,
    #[regex(r"0[fh]")]
    #[regex(r"[1-9][0-9]*[fh]")]
    #[regex(r"[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?[fh]?", priority = 5)]
    #[regex(r"[0-9]+\.[0-9]*([eE][+-]?[0-9]+)?[fh]?")]
    #[regex(r"[0-9]+[eE][+-]?[0-9]+[fh]?")]
    #[regex(
        r"0[xX][0-9a-fA-F]*\.[0-9a-fA-F]+([pP][+-]?[0-9]+[fh]?)?",
        priority = 9
    )]
    #[regex(r"0[xX][0-9a-fA-F]+\.[0-9a-fA-F]*([pP][+-]?[0-9]+[fh]?)?")]
    #[regex(r"0[xX][0-9a-fA-F]+[pP][+-]?[0-9]+[fh]?")]
    FloatLiteral,
    #[regex(r"0[iu]?")]
    #[regex(r"[1-9][0-9]*[iu]?")]
    #[regex(r"0[xX][0-9a-fA-F]+[iu]?")]
    IntLiteral,
    #[regex("[\x20\x09\x0A-\x0D\u{0085}\u{200E}\u{200F}\u{2028}\u{2029}]+")]
    Whitespace,
    #[token("//", lex_line_comment)]
    #[token("/*", lex_block_comment)]
    Comment,
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
