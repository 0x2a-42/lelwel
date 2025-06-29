use super::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use herring::{Herring, Lexer};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
    UnterminatedString,
    Indent,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_label(Label::primary((), span)),
            Self::UnterminatedString => Diagnostic::error()
                .with_message("unterminated string")
                .with_label(Label::primary((), span)),
            Self::Indent => Diagnostic::error()
                .with_message("unindent does not match any outer indentation level")
                .with_label(Label::primary((), span)),
        }
    }
}

fn lpar(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren += 1;
    Ok(Token::LPar)
}
fn rpar(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren = lexer.extras.paren.saturating_sub(1);
    Ok(Token::RPar)
}
fn lbrak(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren += 1;
    Ok(Token::LBrak)
}
fn rbrak(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren = lexer.extras.paren.saturating_sub(1);
    Ok(Token::RBrak)
}
fn lbrace(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren += 1;
    Ok(Token::LBrace)
}
fn rbrace(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    lexer.extras.paren = lexer.extras.paren.saturating_sub(1);
    Ok(Token::RBrace)
}
fn parse_short_double_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                lexer.bump(1);
                return Ok(Token::String);
            }
            '\\' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            '\n' => {
                break;
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedString)
}
fn parse_short_single_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '\'' => {
                lexer.bump(1);
                return Ok(Token::String);
            }
            '\\' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            '\n' => {
                break;
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedString)
}
fn parse_long_double_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    let mut closing = 0;
    while let Some(c) = it.next() {
        match c {
            '"' if closing == 2 => {
                lexer.bump(1);
                return Ok(Token::String);
            }
            '"' => {
                lexer.bump(1);
                closing += 1;
            }
            '\\' => {
                closing = 0;
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            c => {
                closing = 0;
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedString)
}
fn parse_long_single_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    let mut closing = 0;
    while let Some(c) = it.next() {
        match c {
            '\'' if closing == 2 => {
                lexer.bump(1);
                return Ok(Token::String);
            }
            '\'' => {
                lexer.bump(1);
                closing += 1;
            }
            '\\' => {
                closing = 0;
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            c => {
                closing = 0;
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedString)
}
fn check_indent(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    if lexer.remainder().starts_with('\n')
        || lexer.remainder().starts_with('#')
        || lexer.remainder().starts_with("\r\n")
    {
        return Ok(Token::Whitespace);
    }
    if lexer.extras.paren == 0 {
        let mut error = false;
        let mut indent = 0;
        let nl_index = lexer.slice().find('\n').unwrap();
        for c in lexer.slice()[nl_index + 1..].chars() {
            if c == '\t' {
                indent += 8 - (indent % 8);
            } else {
                indent += 1;
            }
        }
        if indent > *lexer.extras.indent.last().unwrap() {
            lexer.extras.pending_indent += 1;
            lexer.extras.indent.push(indent);
        } else {
            let mut count = 0;
            for level in lexer.extras.indent.iter().rev() {
                if *level == indent {
                    break;
                }
                if *level < indent {
                    error = true;
                    break;
                }
                count += 1;
            }
            lexer.extras.pending_dedent += count;
            lexer
                .extras
                .indent
                .truncate(lexer.extras.indent.len() - count);
        }
        if error {
            Err(LexerError::Indent)
        } else {
            Ok(Token::Newline)
        }
    } else {
        Ok(Token::Whitespace)
    }
}
fn check_first_line_indent(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    if lexer.span().start == 0 {
        lexer.extras.pending_indent += 1;
        lexer.extras.indent.push(lexer.span().end);
    }
    Ok(Token::Whitespace)
}
fn emit_indent_dedent(lexer: &mut Lexer<'_, Token>) -> Option<Result<Token, LexerError>> {
    if lexer.extras.pending_indent > 0 {
        lexer.extras.pending_indent -= 1;
        return Some(Ok(Token::Indent));
    }
    if lexer.extras.pending_dedent > 0 {
        lexer.extras.pending_dedent -= 1;
        return Some(Ok(Token::Dedent));
    }
    None
}

pub struct Context {
    paren: usize,
    indent: Vec<usize>,
    pending_indent: usize,
    pending_dedent: usize,
}
impl Default for Context {
    fn default() -> Self {
        Context {
            paren: 0,
            indent: vec![0],
            pending_indent: 0,
            pending_dedent: 0,
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Herring, Debug, PartialEq, Copy, Clone)]
#[herring(initial = emit_indent_dedent)]
#[herring(extras = Context)]
#[herring(error = LexerError)]
#[herring(subpattern stringprefix = "r|u|ur|R|U|UR|Ur|uR|b|B|br|Br|bR|BR")]
#[herring(subpattern exponent = "[eE][+-]?[0-9]+")]
#[herring(subpattern pointfloat = r"[0-9]*\.[0-9]+|[0-9]+\.")]
#[herring(subpattern floatnumber = "(?&pointfloat)(?&exponent)?|[0-9]+(?&exponent)")]
pub enum Token {
    EOF,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("=")]
    Eq,
    #[token("@")]
    At,
    #[token("&")]
    And,
    #[token(".")]
    Dot,
    #[token("(", lpar)]
    LPar,
    #[token(")", rpar)]
    RPar,
    #[token("*")]
    Star,
    #[token("+")]
    Plus,
    #[token(";")]
    Semi,
    #[token("<<")]
    Lt2,
    #[token("==")]
    Eq2,
    #[token(">>")]
    Gt2,
    #[token("|")]
    Pipe,
    #[token(",")]
    Comma,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token(":")]
    Colon,
    #[token("<=")]
    LtEq,
    #[token("<>")]
    LtGt,
    #[token(">=")]
    GtEq,
    #[token("[", lbrak)]
    LBrak,
    #[token("]", rbrak)]
    RBrak,
    #[token("^")]
    Caret,
    #[token("`")]
    BTick,
    #[token("~")]
    Tilde,
    #[token("&=")]
    AndEq,
    #[token("**")]
    Star2,
    #[token("{", lbrace)]
    LBrace,
    #[token("}", rbrace)]
    RBrace,
    #[token("!=")]
    ExclEq,
    #[token("%")]
    Percent,
    #[token("*=")]
    StarEq,
    #[token("+=")]
    PlusEq,
    #[token("//")]
    Slash2,
    #[token("<<=")]
    Lt2Eq,
    #[token(">>=")]
    Gt2Eq,
    #[token("|=")]
    PipeEq,
    #[token("-=")]
    MinusEq,
    #[token("/=")]
    SlashEq,
    #[token("^=")]
    CaretEq,
    #[token("**=")]
    Star2Eq,
    #[token("%=")]
    PercentEq,
    #[token("//=")]
    Slash2Eq,
    #[token("as")]
    As,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("is")]
    Is,
    #[token("or")]
    Or,
    #[token("and")]
    AndKw,
    #[token("def")]
    Def,
    #[token("del")]
    Del,
    #[token("for")]
    For,
    #[token("not")]
    Not,
    #[token("try")]
    Try,
    #[token("else")]
    Else,
    #[token("exec")]
    Exec,
    #[token("from")]
    From,
    #[token("pass")]
    Pass,
    #[token("elif")]
    Elif,
    #[token("with")]
    With,
    #[token("break")]
    Break,
    #[token("class")]
    Class,
    #[token("print")]
    Print,
    #[token("raise")]
    Raise,
    #[token("while")]
    While,
    #[token("assert")]
    Assert,
    #[token("except")]
    Except,
    #[token("global")]
    Global,
    #[token("import")]
    Import,
    #[token("lambda")]
    Lambda,
    #[token("return")]
    Return,
    #[token("finally")]
    Finally,
    #[token("continue")]
    Continue,
    #[token("yield")]
    Yield,
    #[regex("\r?\n[\t ]*", check_indent)]
    Newline,
    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Name,
    Dedent,
    Indent,
    #[regex("([1-9][0-9]*|0)[lL]?")]
    #[regex("(0[oO][0-7]+|0[0-7]+)[lL]?")]
    #[regex("0[xX][0-9a-fA-F]+[lL]?")]
    #[regex("0[bB][0-1]+[lL]?")]
    Int,
    #[regex(r"(?&floatnumber)")]
    Float,
    #[regex(r"(?&floatnumber)[jJ]")]
    #[regex(r"[0-9]+[jJ]")]
    Imaginary,
    #[regex("(?&stringprefix)?\"", parse_short_double_string)]
    #[regex("(?&stringprefix)?'", parse_short_single_string)]
    #[regex("(?&stringprefix)?\"\"\"", parse_long_double_string)]
    #[regex("(?&stringprefix)?'''", parse_long_single_string)]
    String,
    #[regex(r"([\t ]|\\\r?\n)+", check_first_line_indent)]
    Whitespace,
    #[regex("#[^\n]*")]
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
                tokens.push(match err {
                    LexerError::Invalid => Token::Error,
                    LexerError::UnterminatedString => Token::String,
                    LexerError::Indent => Token::Newline,
                });
                diags.push(err.into_diagnostic(span.clone()));
            }
        }
        spans.push(span);
    }
    (tokens, spans)
}
