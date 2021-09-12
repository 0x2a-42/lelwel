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
                write!(f, "Undefined element '{}'", sym)
            }
            Code::UppercaseRule(sym) => {
                write!(f, "Rule '{}' must start with lowercase letter", sym)
            }
            Code::LowercaseToken(sym) => {
                write!(f, "Token '{}' must start with uppercase letter or '_'", sym)
            }
            Code::Redefinition(sym) => {
                write!(f, "Redefinition of {}", sym)
            }
            Code::InvalidLang(sym) => {
                write!(f, "Invalid target language '{}'", sym)
            }
            Code::ExpectedAction(num) => {
                write!(f, "Expected action with number {} or {}", num - 1, num)
            }
            Code::ExpectedPredicate(num) => {
                if *num == 1 {
                    write!(f, "Expected predicate with number {}", num)
                } else {
                    write!(f, "Expected predicate with number {} or {}", num - 1, num)
                }
            }
            Code::ExpectedErrorHandler(num) => {
                if *num == 1 {
                    write!(f, "Expected error handler with number {}", num)
                } else {
                    write!(
                        f,
                        "Expected error handler with number {} or {}",
                        num - 1,
                        num
                    )
                }
            }
            Code::UndefinedAction => {
                write!(f, "Undefined action")
            }
            Code::UndefinedPredicate => {
                write!(f, "Undefined predicate")
            }
            Code::UndefinedErrorHandler => {
                write!(f, "Undefined error handler")
            }
            Code::UnusedElement => {
                write!(f, "Unused element")
            }
            Code::MissingStart => {
                write!(f, "Missing start rule")
            }
            Code::PredPosition => {
                write!(f, "Predicate must be first term in alternation branch")
            }
            Code::LL1Conflict => {
                write!(f, "LL(1) confilict")
            }
            Code::ConsumeTokens => {
                write!(f, "No tokens consumed")
            }
            Code::ErrorSyntax => {
                write!(f, "Error handler must be only term in alternation branch or last term of concatenation")
            }
            Code::ErrorCount => {
                write!(f, "Error handler can only occur in one alternation branch")
            }
            Code::PredefToken => {
                write!(f, "Cannot use predefined token name")
            }
        }
    }
}
