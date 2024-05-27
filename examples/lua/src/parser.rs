use codespan_reporting::diagnostic::Label;
use logos::{Lexer, Logos};

use crate::ast::{AstNode, Exp};

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
        .map(|i| lexer.bump(i + prefix_len))
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
    #[token("#")]
    Hash,
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
    #[regex(r"\d+\.\d*([eE][+-]?\d+)?")]
    #[regex(r"\.\d+([eE][+-]?\d+)?")]
    #[regex(r"\d+[eE][+-]?\d+")]
    #[regex(r"0[xX][a-fA-F0-9]+\.[a-fA-F0-9]*([pP][+-]?[a-fA-F0-9]+)?")]
    #[regex(r"0[xX]\.[a-fA-F0-9]+([pP][+-]?[a-fA-F0-9]+)?")]
    #[regex(r"0[xX][a-fA-F0-9]+[pP][+-]?[a-fA-F0-9]+")]
    Numeral,
    Error,
}

type CstIndex = usize;

#[derive(Default)]
struct Context<'a> {
    marker: std::marker::PhantomData<&'a ()>,
}

pub fn tokenize(
    lexer: logos::Lexer<Token>,
    diags: &mut Vec<Diagnostic>,
) -> (Vec<Token>, Vec<std::ops::Range<CstIndex>>) {
    let mut tokens = vec![];
    let mut ranges = vec![];

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
        ranges.push(span.start as CstIndex..span.end as CstIndex);
    }
    (tokens, ranges)
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl<'a> Parser<'a> {
    fn build(&mut self, rule: Rule, node: NodeRef, diags: &mut Vec<Diagnostic>) {
        match rule {
            Rule::Expstat => {
                self.cst.children(node).for_each(|c| {
                    if let Some(exp) = Exp::cast(&self.cst, c) {
                        match exp {
                            Exp::Callexp(_) => {}
                            _ => {
                                diags.push(
                                    Diagnostic::error()
                                        .with_message("unexpected expression kind")
                                        .with_labels(vec![
                                            codespan_reporting::diagnostic::Label::primary(
                                                (),
                                                self.cst.get_span(c).unwrap(),
                                            ),
                                        ])
                                        .with_notes(vec![
                                            "note: expected call expression".to_string()
                                        ]),
                                );
                            }
                        }
                    }
                });
            }
            Rule::Assignstat => {
                self.cst.children(node).for_each(|c| {
                    if let Some(exp) = Exp::cast(&self.cst, c) {
                        match exp {
                            Exp::Nameexp(_) | Exp::Indexexp(_) | Exp::Fieldexp(_) => {}
                            _ => {
                                diags.push(
                                    Diagnostic::error()
                                        .with_message("unexpected expression kind")
                                        .with_labels(vec![
                                            codespan_reporting::diagnostic::Label::primary(
                                                (),
                                                self.cst.get_span(c).unwrap(),
                                            ),
                                        ])
                                        .with_notes(vec![
                                            "note: expected name, index, or field expression"
                                                .to_string(),
                                        ]),
                                );
                            }
                        }
                    }
                });
            }
            Rule::Attrib => {
                self.cst
                    .children(node)
                    .find_map(|node| self.cst.get_token(node, Token::Name))
                    .inspect(|(value, span)| {
                        if *value != "const" && *value != "close" {
                            diags.push(
                                Diagnostic::error()
                                    .with_message(format!("unexpected attribute name: '{}'", value))
                                    .with_labels(vec![
                                        codespan_reporting::diagnostic::Label::primary(
                                            (),
                                            span.clone(),
                                        ),
                                    ])
                                    .with_notes(vec![
                                        "note: expected 'const' or 'close'".to_string()
                                    ]),
                            );
                        }
                    });
            }
            _ => {}
        }
    }
    fn predicate_forstat_1(&self) -> bool {
        self.peek(1) == Token::Equal
    }
    fn predicate_pars_1(&self) -> bool {
        self.peek(1) != Token::Ellipsis
    }
    fn predicate_prefixexp_1(&self) -> bool {
        true
    }
    fn predicate_fieldlist_1(&self) -> bool {
        self.peek(1) != Token::RBrace
    }
    fn predicate_field_1(&self) -> bool {
        self.peek(1) == Token::Equal
    }
}
