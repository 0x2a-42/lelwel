use crate::parser::{Diagnostic, Span};
use codespan_reporting::diagnostic::Label;
use herring::{Herring, Lexer};

#[derive(Debug, Clone, PartialEq, Default)]
pub enum LexerError {
    #[default]
    Invalid,
    UnterminatedBasicString,
    UnterminatedLiteralString,
    UnterminatedMlBasicString,
    UnterminatedMlLiteralString,
}

impl LexerError {
    pub fn into_diagnostic(self, span: Span) -> Diagnostic {
        match self {
            Self::Invalid => Diagnostic::error()
                .with_message("invalid token")
                .with_labels(vec![Label::primary((), span)]),
            Self::UnterminatedBasicString => Diagnostic::error()
                .with_message("unterminated basic string")
                .with_labels(vec![Label::primary((), span)]),
            Self::UnterminatedLiteralString => Diagnostic::error()
                .with_message("unterminated literal string")
                .with_labels(vec![Label::primary((), span)]),
            Self::UnterminatedMlBasicString => Diagnostic::error()
                .with_message("unterminated multi-line basic string")
                .with_labels(vec![Label::primary((), span)]),
            Self::UnterminatedMlLiteralString => Diagnostic::error()
                .with_message("unterminated multi-line literal string")
                .with_labels(vec![Label::primary((), span)]),
        }
    }
}

fn parse_basic_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    while let Some(c) = it.next() {
        match c {
            '"' => {
                lexer.bump(1);
                return Ok(Token::BasicString);
            }
            '\\' => {
                lexer.bump(1);
                if let Some(c) = it.next() {
                    lexer.bump(c.len_utf8());
                }
            }
            '\r' | '\n' => {
                break;
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedBasicString)
}
fn parse_ml_basic_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut it = lexer.remainder().chars();
    let mut closing = 0;
    while let Some(c) = it.next() {
        match c {
            '"' if closing < 5 => {
                lexer.bump(1);
                closing += 1;
            }
            _ if closing >= 3 => {
                return Ok(Token::MlBasicString);
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
    Err(LexerError::UnterminatedMlBasicString)
}
fn parse_literal_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    for c in lexer.remainder().chars() {
        match c {
            '\'' => {
                lexer.bump(1);
                return Ok(Token::LiteralString);
            }
            '\r' | '\n' => {
                break;
            }
            c => {
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedLiteralString)
}
fn parse_ml_literal_string(lexer: &mut Lexer<'_, Token>) -> Result<Token, LexerError> {
    let mut closing = 0;
    for c in lexer.remainder().chars() {
        match c {
            '\'' if closing < 5 => {
                lexer.bump(1);
                closing += 1;
            }
            _ if closing >= 3 => {
                return Ok(Token::MlLiteralString);
            }
            c => {
                closing = 0;
                lexer.bump(c.len_utf8());
            }
        }
    }
    Err(LexerError::UnterminatedMlLiteralString)
}

fn check_escape(
    it: &mut impl Iterator<Item = (usize, char)>,
    value: &str,
    span: &Span,
    diags: &mut Vec<Diagnostic>,
    is_ml: bool,
) {
    match it.next() {
        Some((_, '"' | '\\' | 'b' | 'f' | 'n' | 'r' | 't')) => {}
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
                    return;
                }
            }
            let val = u32::from_str_radix(&value[i + 1..i + 5], 16).unwrap();
            if val > 0xD7FF && val < 0xE000 {
                diags.push(
                    Diagnostic::error()
                        .with_message("non scalar unicode code point")
                        .with_labels(vec![Label::primary(
                            (),
                            span.start + i - 1..span.start + i + 5,
                        )]),
                );
            }
        }
        Some((i, 'U')) => {
            for j in 0..8 {
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
                    return;
                }
            }
            let val = u32::from_str_radix(&value[i + 1..i + 9], 16).unwrap();
            if val > 0xD7FF && val < 0xE000 {
                diags.push(
                    Diagnostic::error()
                        .with_message("non scalar unicode code point")
                        .with_labels(vec![Label::primary(
                            (),
                            span.start + i - 1..span.start + i + 9,
                        )]),
                );
            } else if val > 0x10FFFF {
                diags.push(
                    Diagnostic::error()
                        .with_message("out of range unicode escape sequence")
                        .with_labels(vec![Label::primary(
                            (),
                            span.start + i - 1..span.start + i + 9,
                        )]),
                );
            }
        }
        Some((i, mut c @ ('\n' | '\r' | '\x20' | '\x09'))) if is_ml => {
            let mut j = i;
            while c != '\n' {
                if c == '\x20' || c == '\x09' {
                    if let Some((k, n)) = it.next() {
                        c = n;
                        j = k;
                        continue;
                    }
                }
                if c == '\r' {
                    if let Some((k, n)) = it.next() {
                        if n == '\n' {
                            break;
                        }
                        j = k;
                    }
                }
                diags.push(
                    Diagnostic::error()
                        .with_message("invalid escape sequence")
                        .with_labels(vec![Label::primary(
                            (),
                            span.start + i - 1..span.start + j + 1,
                        )]),
                );
                return;
            }
        }
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
fn check_string(value: &str, span: &Span, diags: &mut Vec<Diagnostic>, is_ml: bool, is_lit: bool) {
    let mut it = value.chars().enumerate();
    while let Some((i, c)) = it.next() {
        match c {
            '\\' if !is_lit => check_escape(&mut it, value, span, diags, is_ml),
            '\r' => {
                if let Some((_, c)) = it.next() {
                    if c == '\n' {
                        continue;
                    }
                }
                diags.push(
                    Diagnostic::error()
                        .with_message(format!("string contains invalid character {c:?}"))
                        .with_labels(vec![
                            Label::primary((), span.start + i..span.start + i + 1)
                                .with_message("after this character"),
                        ]),
                );
            }
            '\''
            | '\n'
            | '\x09'
            | '\x20'..='\x26'
            | '\x28'..='\x7E'
            | '\u{80}'..='\u{D7FF}'
            | '\u{E000}'..='\u{10FFFF}'
                if is_lit => {}
            '"'
            | '\n'
            | '\x20'
            | '\x09'
            | '\x21'
            | '\x23'..='\x5B'
            | '\x5D'..='\x7E'
            | '\u{80}'..='\u{D7FF}'
            | '\u{E000}'..='\u{10FFFF}'
                if !is_lit => {}
            c => {
                diags.push(
                    Diagnostic::error()
                        .with_message(format!("string contains invalid character {c:?}"))
                        .with_labels(vec![
                            Label::primary((), span.start + i..span.start + i + 1)
                                .with_message("after this character"),
                        ]),
                );
            }
        }
    }
}
fn check_time(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let mut it = value.split('.').next().unwrap().split(':');
    let hours = it.next().unwrap().parse::<u8>().unwrap();
    if hours > 23 {
        diags.push(
            Diagnostic::error()
                .with_message("invalid hour")
                .with_labels(vec![Label::primary((), span.start..span.start + 2)]),
        );
    }
    let minutes = it.next().unwrap().parse::<u8>().unwrap();
    if minutes > 59 {
        diags.push(
            Diagnostic::error()
                .with_message("invalid minute")
                .with_labels(vec![Label::primary((), span.start + 3..span.start + 5)]),
        );
    }
    if let Some(seconds) = it.next().and_then(|val| val.parse::<u8>().ok()) {
        // don't check for leap seconds
        if seconds > 60 {
            diags.push(
                Diagnostic::error()
                    .with_message("invalid second")
                    .with_labels(vec![Label::primary((), span.start + 6..span.start + 8)]),
            );
        }
    }
}
fn check_local_date(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let mut it = value.split('-');
    let year = it.next().unwrap().parse::<u16>().unwrap();
    let month = it.next().unwrap().parse::<u8>().unwrap();
    let day = it.next().unwrap().parse::<u8>().unwrap();

    let is_leap_year = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0);

    let max_days = match month {
        1 => 31,
        2 if is_leap_year => 29,
        2 => 28,
        3 => 31,
        4 => 30,
        5 => 31,
        6 => 30,
        7 => 31,
        8 => 31,
        9 => 30,
        10 => 31,
        11 => 30,
        12 => 31,
        _ => {
            diags.push(
                Diagnostic::error()
                    .with_message("invalid month")
                    .with_labels(vec![Label::primary((), span.start + 5..span.start + 7)]),
            );
            31
        }
    };
    if day < 1 || day > max_days {
        diags.push(
            Diagnostic::error()
                .with_message("invalid day")
                .with_labels(vec![Label::primary((), span.start + 8..span.start + 10)]),
        );
    }
}
fn check_local_date_time(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let time_split = value.find(['t', 'T', ' ']).unwrap();
    let (local_date, local_time) = value.split_at(time_split);
    check_local_date(local_date, &(span.start..span.start + time_split), diags);
    check_time(
        &local_time[1..],
        &(span.start + time_split + 1..span.end),
        diags,
    );
}
fn check_offset_date_time(value: &str, span: &Span, diags: &mut Vec<Diagnostic>) {
    let time_split = value.find(['t', 'T', ' ']).unwrap();
    let (local_date, time) = value.split_at(time_split);
    let offset_split = time.find(['z', 'Z', '+', '-']).unwrap();
    let (local_time, offset_time) = time.split_at(offset_split);
    check_local_date(local_date, &(span.start..span.start + time_split), diags);
    check_time(
        &local_time[1..],
        &(span.start + time_split + 1..span.start + offset_split),
        diags,
    );
    if offset_time.len() > 1 {
        check_time(
            &offset_time[1..],
            &(span.start + offset_split + 1..span.end),
            diags,
        );
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Herring, Debug, PartialEq, Copy, Clone)]
#[herring(error = LexerError)]
#[herring(subpattern full_date = "[0-9]{4}-[0-9]{2}-[0-9]{2}")]
#[herring(subpattern partial_time = r"[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?")]
#[herring(subpattern full_time = r"(?&partial_time)([zZ]|[+-][0-9]{2}:[0-9]{2})")]
#[herring(subpattern zero_prefixable_int = "[0-9]([0-9]|_[0-9])*")]
#[herring(subpattern dec_int = "[-+]?([0-9]|[1-9]([0-9]|_[0-9])+)")]
#[herring(subpattern exp = "[eE][-+]?(?&zero_prefixable_int)")]
pub enum Token {
    EOF,
    #[token(".")]
    Dot,
    #[token("=")]
    Eq,
    #[token("[")]
    LBrak,
    #[token("]")]
    RBrak,
    #[token(",")]
    Comma,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("\"", parse_basic_string)]
    BasicString,
    #[token("'", parse_literal_string)]
    LiteralString,
    #[regex("\"\"\"", parse_ml_basic_string)]
    MlBasicString,
    #[regex("'''", parse_ml_literal_string)]
    MlLiteralString,
    #[regex("(?&dec_int)")]
    #[regex("0x[0-9a-fA-F]([0-9a-fA-F]|_[0-9a-fA-F])*")]
    #[regex("0o[0-7]([0-7]|_[0-7])*")]
    #[regex("0b[01]([01]|_[01])*")]
    Integer,
    #[regex(r"(?&dec_int)((?&exp)|\.(?&zero_prefixable_int)(?&exp)?)")]
    #[regex("[+-]?inf")]
    #[regex("[+-]?nan")]
    Float,
    #[regex("(?&full_date)[tT ](?&full_time)")]
    OffsetDateTime,
    #[regex("(?&full_date)[tT ](?&partial_time)")]
    LocalDateTime,
    #[regex("(?&full_date)")]
    LocalDate,
    #[regex("(?&partial_time)")]
    LocalTime,
    #[regex("[A-Za-z0-9_-]+", priority = 0)]
    UnquotedKey,
    #[regex("\r?\n")]
    Newline,
    #[regex("[\u{0020}\u{0009}]+")]
    Whitespace,
    #[regex("#[\x09\x20-\x7E\u{0080}-\u{D7FF}\u{E000}-\u{10FFFF}]*")]
    Comment,
    Error,
}

enum Context {
    Array,
    Table,
}
fn is_key_context(token: Token, context: &mut Vec<Context>, was_key_context: bool) -> bool {
    match token {
        Token::LBrace => {
            context.push(Context::Table);
            true
        }
        Token::RBrace => {
            context.pop();
            false
        }
        Token::LBrak => {
            if !was_key_context {
                context.push(Context::Array);
                false
            } else {
                context.is_empty()
            }
        }
        Token::RBrak => {
            context.pop();
            false
        }
        Token::Comma => {
            matches!(context.last(), Some(Context::Table))
        }
        Token::Dot => true,
        Token::Newline => {
            if matches!(context.last(), Some(Context::Table)) {
                context.pop();
            }
            context.is_empty()
        }
        Token::Whitespace | Token::Error => was_key_context,
        _ => false,
    }
}
fn map_to_key(token: Token, lexer: &mut Lexer<'_, Token>, diags: &mut Vec<Diagnostic>) -> Token {
    match token {
        Token::Integer | Token::LocalDate | Token::True | Token::False => Token::UnquotedKey,
        Token::Float => {
            let slice = lexer.slice();
            if slice.starts_with('+') {
                let span = lexer.span();
                lexer.offset = span.start + 1;
                diags.push(LexerError::Invalid.into_diagnostic(span.start..span.start + 1));
                Token::Error
            } else {
                if let Some(offset) = slice.find(['.', '+']) {
                    lexer.offset = lexer.span().start + offset;
                }
                Token::UnquotedKey
            }
        }
        _ => token,
    }
}

pub fn tokenize(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
    let mut lexer = Token::lexer(source);
    let mut tokens = vec![];
    let mut spans = vec![];
    let mut context = vec![];
    let mut in_key_context = true;

    while let Some(token) = lexer.next() {
        match token {
            Ok(token) => {
                let token = if in_key_context {
                    map_to_key(token, &mut lexer, diags)
                } else {
                    token
                };
                let span = lexer.span();
                in_key_context = is_key_context(token, &mut context, in_key_context);
                match token {
                    Token::BasicString => {
                        check_string(&source[span.start..span.end], &span, diags, false, false)
                    }
                    Token::MlBasicString => {
                        check_string(&source[span.start..span.end], &span, diags, true, false)
                    }
                    Token::LiteralString => {
                        check_string(&source[span.start..span.end], &span, diags, false, true)
                    }
                    Token::MlLiteralString => {
                        check_string(&source[span.start..span.end], &span, diags, true, true)
                    }
                    Token::LocalTime => {
                        check_time(&source[span.start..span.end], &span, diags);
                    }
                    Token::LocalDate => {
                        check_local_date(&source[span.start..span.end], &span, diags);
                    }
                    Token::LocalDateTime => {
                        check_local_date_time(&source[span.start..span.end], &span, diags);
                    }
                    Token::OffsetDateTime => {
                        check_offset_date_time(&source[span.start..span.end], &span, diags);
                    }
                    _ => {}
                }
                tokens.push(token);
            }
            Err(err) => {
                tokens.push(match err {
                    LexerError::Invalid => Token::Error,
                    LexerError::UnterminatedBasicString => Token::BasicString,
                    LexerError::UnterminatedLiteralString => Token::LiteralString,
                    LexerError::UnterminatedMlBasicString => Token::MlBasicString,
                    LexerError::UnterminatedMlLiteralString => Token::MlLiteralString,
                });
                diags.push(err.into_diagnostic(lexer.span()));
            }
        }
        spans.push(lexer.span());
    }
    (tokens, spans)
}
