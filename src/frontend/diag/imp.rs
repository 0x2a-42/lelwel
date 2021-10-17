use super::super::parser::*;
use super::super::sema::*;
use super::super::symbol::*;

#[derive(Debug)]
pub enum Code {
    SyntaxError(Vec<TokenKind>),
    ParserError(&'static str),
    UndefinedElement(Symbol),
    UppercaseRule(Symbol),
    LowercaseToken(Symbol),
    Redefinition(Binding),
    InvalidLang(Symbol),
    ExpectedAction(u64),
    ExpectedPredicate(u64),
    ExpectedErrorHandler(u64),
    UndefinedAction,
    UndefinedPredicate,
    UndefinedErrorHandler,
    UnusedElement,
    MissingStart,
    PredPosition,
    LL1Conflict,
    ConsumeTokens,
    ErrorSyntax,
    ErrorCount,
    PredefToken,
}

impl From<Vec<TokenKind>> for Code {
    fn from(error: Vec<TokenKind>) -> Code {
        Code::SyntaxError(error)
    }
}

impl From<&'static str> for Code {
    fn from(error: &'static str) -> Code {
        Code::ParserError(error)
    }
}

impl std::fmt::Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Code::SyntaxError(expected) => {
                if expected.len() > 1 {
                    write!(f, "invalid syntax, expected one of: ")?;
                } else {
                    write!(f, "invalid syntax, expected: ")?;
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
                    if count < expected.len() {
                        write!(f, "{}, ", s)?;
                    } else {
                        write!(f, "{}", s)?;
                    }
                }
                write!(f, ".")
            }
            Code::ParserError(msg) => {
                write!(f, "{}", msg)
            }
            Code::UndefinedElement(sym) => {
                write!(f, "undefined element '{}'", sym)
            }
            Code::UppercaseRule(sym) => {
                write!(f, "rule '{}' must start with lowercase letter", sym)
            }
            Code::LowercaseToken(sym) => {
                write!(f, "token '{}' must start with uppercase letter or '_'", sym)
            }
            Code::Redefinition(sym) => {
                write!(f, "redefinition of {}", sym)
            }
            Code::InvalidLang(sym) => {
                write!(f, "invalid target language '{}'", sym)
            }
            Code::ExpectedAction(num) => {
                write!(f, "expected action with number {} or {}", num - 1, num)
            }
            Code::ExpectedPredicate(num) => {
                if *num == 1 {
                    write!(f, "expected predicate with number {}", num)
                } else {
                    write!(f, "expected predicate with number {} or {}", num - 1, num)
                }
            }
            Code::ExpectedErrorHandler(num) => {
                if *num == 1 {
                    write!(f, "expected error handler with number {}", num)
                } else {
                    write!(
                        f,
                        "expected error handler with number {} or {}",
                        num - 1,
                        num
                    )
                }
            }
            Code::UndefinedAction => {
                write!(f, "undefined action")
            }
            Code::UndefinedPredicate => {
                write!(f, "undefined predicate")
            }
            Code::UndefinedErrorHandler => {
                write!(f, "undefined error handler")
            }
            Code::UnusedElement => {
                write!(f, "unused element")
            }
            Code::MissingStart => {
                write!(f, "missing start rule")
            }
            Code::PredPosition => {
                write!(f, "predicate must be first term in alternation branch")
            }
            Code::LL1Conflict => {
                write!(f, "LL(1) confilict")
            }
            Code::ConsumeTokens => {
                write!(f, "no tokens consumed")
            }
            Code::ErrorSyntax => {
                write!(f, "error handler must be only term in alternation branch or last term of concatenation")
            }
            Code::ErrorCount => {
                write!(f, "error handler can only occur in one alternation branch")
            }
            Code::PredefToken => {
                write!(f, "cannot use predefined token name")
            }
        }
    }
}
