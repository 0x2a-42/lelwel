use super::super::parser::*;

#[derive(Debug)]
pub enum Code {
    SyntaxError(Vec<TokenKind>),
    ParserError(&'static str),
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
        }
    }
}
