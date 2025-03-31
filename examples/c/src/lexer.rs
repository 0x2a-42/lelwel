use crate::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
    UnterminatedString,
    UnterminatedChar,
    UnterminatedComment,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedString => Diagnostic::error()
                .with_message("unterminated string literal")
                .with_labels(vec![Label::primary((), span)]),
            LexerError::UnterminatedChar => Diagnostic::error()
                .with_message("unterminated character constant")
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
fn parse_char(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '\'' => {
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
    Err(LexerError::UnterminatedChar)
}
fn parse_line_comment(lexer: &mut Lexer<'_, Token>) -> Result<(), LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '\n' => {
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
    Err(LexerError::UnterminatedComment)
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
#[logos(subpattern hex_quad = r"[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F]")]
#[logos(error = LexerError)]
pub enum Token {
    EOF,
    #[regex(r"[a-zA-Z_]([a-zA-Z0-9_]|\\u(?&hex_quad)|\\U(?&hex_quad)(?&hex_quad))*")]
    Identifier,
    #[regex(
        "([1-9][0-9]*|0[0-7]*|0[xX][0-9a-fA-F]*)([uU][lL]?|[uU](ll|LL)?|[lL][uU]?|(ll|LL)[uU]?)?"
    )]
    IntConst,
    #[regex(r"(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+([eE][+-]?[0-9]+))[flFL]?")]
    #[regex(r"0[xX]([0-9a-fA-F]*\.[0-9a-fA-F]+|[0-9a-fA-F]+\.?)([pP][+-]?[0-9]+)?[flFL]?")]
    // GNU extension for suffix
    #[regex(r"(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+([eE][+-]?[0-9]+))[fF]16")]
    #[regex(r"(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+([eE][+-]?[0-9]+))[iIjJ]([fF](16)?|[lL])?")]
    #[regex(r"(([0-9]*\.[0-9]+|[0-9]+\.)([eE][+-]?[0-9]+)?|[0-9]+([eE][+-]?[0-9]+))([fF](16)?|[lL])[iIjJ]")]
    #[regex(r"0[xX]([0-9a-fA-F]*\.[0-9a-fA-F]+|[0-9a-fA-F]+\.?)([pP][+-]?[0-9]+)?[fF]16")]
    #[regex(r"0[xX]([0-9a-fA-F]*\.[0-9a-fA-F]+|[0-9a-fA-F]+\.?)([pP][+-]?[0-9]+)?[iIjJ]([fF](16)?|[lL])?")]
    #[regex(r"0[xX]([0-9a-fA-F]*\.[0-9a-fA-F]+|[0-9a-fA-F]+\.?)([pP][+-]?[0-9]+)?([fF](16)?|[lL])[iIjJ]")]
    FloatConst,
    #[regex("[LuU]?'", parse_char)]
    CharConst,
    #[regex("(u8|u|U|L)?\"", parse_string)]
    StringLiteral,
    #[token("void")]
    Void,
    #[token("char")]
    Char,
    #[token("short")]
    Short,
    #[token("int")]
    Int,
    #[token("__int128")] // GNU extension
    Int128,
    #[token("long")]
    Long,
    #[token("signed")]
    #[token("__signed__")] // GNU extension
    Signed,
    #[token("unsigned")]
    Unsigned,
    #[token("struct")]
    Struct,
    #[token("union")]
    Union,
    #[token("enum")]
    Enum,
    #[token("float")]
    Float,
    #[token("double")]
    Double,
    #[token("_Bool")]
    Bool,
    #[token("_Complex")]
    #[token("__complex")] // GNU extension
    #[token("__complex__")] // GNU extension
    Complex,
    #[token("_Imaginary")]
    Imaginary,
    #[token("const")]
    #[token("__const")]
    #[token("__const__")]
    Const,
    #[token("volatile")]
    #[token("__volatile__")] // GNU extension
    Volatile,
    #[token("restrict")]
    #[token("__restrict")] // GNU extension
    #[token("__restrict__")] // GNU extension
    Restrict,
    #[token("_Atomic")]
    Atomic,
    #[token("typedef")]
    Typedef,
    #[token("auto")]
    Auto,
    #[token("register")]
    Register,
    #[token("static")]
    Static,
    #[token("extern")]
    Extern,
    #[token("_Thread_local")]
    #[token("__thread")] // GNU extension
    ThreadLocal,
    #[token("inline")]
    #[token("__inline")] // GNU extension
    #[token("__inline__")] // GNU extension
    Inline,
    #[token("_Noreturn")]
    Noreturn,
    #[token("_Alignas")]
    Alignas,
    #[token("_Generic")]
    Generic,
    #[token("_Static_assert")]
    StaticAssert,
    #[token("default")]
    Default,
    #[token("case")]
    Case,
    #[token("sizeof")]
    Sizeof,
    #[token("_Alignof")]
    #[token("__alignof__")] // GNU extension
    Alignof,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("do")]
    Do,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("switch")]
    Switch,
    #[token("goto")]
    Goto,
    #[token("continue")]
    Continue,
    #[token("break")]
    Break,
    #[token("return")]
    Return,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("{")]
    #[token("<%")]
    LBrace,
    #[token("}")]
    #[token("%>")]
    RBrace,
    #[token("[")]
    #[token("<:")]
    LBrak,
    #[token("]")]
    #[token(":>")]
    RBrak,
    #[token("==")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("!=")]
    Neq,
    #[token("=")]
    Assign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("<<=")]
    LtLtAssign,
    #[token(">>=")]
    GtGtAssign,
    #[token("&=")]
    AndAssign,
    #[token("^=")]
    HatAssign,
    #[token("|=")]
    PipeAssign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,
    #[token("~")]
    Tilde,
    #[token("!")]
    Excl,
    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,
    #[token("&")]
    And,
    #[token("|")]
    Pipe,
    #[token(":")]
    Colon,
    #[token("<<")]
    LtLt,
    #[token(">>")]
    GtGt,
    #[token("^")]
    Hat,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    PipePipe,
    #[token("?")]
    Quest,
    #[token("...")]
    Ellipsis,
    #[regex(r"//", parse_line_comment)]
    #[regex(r"/\*", parse_block_comment)]
    Comment,
    #[regex(r"([ \t\n\f]|\\\n)+")]
    Whitespace,
    #[token("#")]
    #[token("%:")]
    Hash,
    #[token("##")]
    #[token("%:%:")]
    HashHash,
    #[token("__attribute")] // GNU extension
    #[token("__attribute__")] // GNU extension
    Attribute,
    #[token("asm")] // GNU extension
    #[token("__asm")] // GNU extension
    #[token("__asm__")] // GNU extension
    Asm,
    #[token("__extension__")] // GNU extension
    Extension,
    #[token("__builtin_va_arg")] // GNU extension
    VaArg,
    #[token("__builtin_offsetof")] // GNU extension
    OffsetOf,
    #[token("typeof")] // GNU extension
    #[token("__typeof")] // GNU extension
    #[token("__typeof__")] // GNU extension
    TypeOf,
    #[token("__builtin_types_compatible_p")] // GNU extension
    TypesCompatible,
    #[token("__real__")] // GNU extension
    #[token("__real")] // GNU extension
    Real,
    #[token("__imag__")] // GNU extension
    #[token("__imag")] // GNU extension
    Imag,
    #[token("__auto_type")]
    AutoType,
    Error,
}

pub fn tokenize(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
    let lexer = Token::lexer(source);
    let mut tokens = vec![];
    let mut spans = vec![];

    let mut count_paren = 0;
    let mut count_brace = 0;
    let mut count_brak = 0;
    for (token, span) in lexer.spanned() {
        match token {
            Ok(token) => {
                match token {
                    Token::LPar => count_paren += 1,
                    Token::RPar => count_paren -= 1,
                    Token::LBrace => count_brace += 1,
                    Token::RBrace => count_brace -= 1,
                    Token::LBrak => count_brak += 1,
                    Token::RBrak => count_brak -= 1,
                    _ => {}
                }
                if count_brace + count_brak + count_paren > 256 {
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
        spans.push(span);
    }
    (tokens, spans)
}
