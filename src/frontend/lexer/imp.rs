use super::super::symbol::*;
use super::*;

impl Lexer {
    pub fn state_start(&mut self) -> Transition {
        match self.consume() {
            Some(c) if c.is_alphabetic() || c == '_' => self.state_id(),
            Some(c) if c.is_ascii_digit() => self.state_int(),
            Some(c) if c.is_whitespace() => self.state_ws(c),
            Some('\'') => self.state_str(),
            Some('{') => self.state_code(),
            Some('?') => self.state_pred(),
            Some('#') => self.state_sema(),
            Some('!') => self.state_error(),
            Some('/') => self.state_slash(),
            Some('*') => self.emit(TokenKind::Star),
            Some('+') => self.emit(TokenKind::Plus),
            Some('|') => self.emit(TokenKind::Or),
            Some('(') => self.emit(TokenKind::LPar),
            Some(')') => self.emit(TokenKind::RPar),
            Some('[') => self.emit(TokenKind::LBrak),
            Some(']') => self.emit(TokenKind::RBrak),
            Some('=') => self.emit(TokenKind::Equal),
            Some(':') => self.emit(TokenKind::Colon),
            Some(';') => self.emit(TokenKind::Semi),
            None => self.emit(TokenKind::EOF),
            _ => self.emit_invalid("invalid token"),
        }
    }
    fn state_ws(&mut self, c: char) -> Transition {
        if c == '\n' {
            self.line();
        }
        loop {
            match self.consume() {
                Some('\n') => self.line(),
                Some(c) if c.is_whitespace() => (),
                None => break,
                _ => {
                    self.backup();
                    break;
                }
            }
        }
        self.ignore()
    }
    fn state_id(&mut self) -> Transition {
        self.accept_star(|c| c.is_alphanumeric() || c == '_');
        let sym = self.get(0, 0).into_symbol();
        match sym {
            Symbol::START => self.emit(TokenKind::Start),
            Symbol::PREAMBLE => self.emit(TokenKind::Preamble),
            Symbol::ERROR => self.emit(TokenKind::Error),
            Symbol::TOKEN => self.emit(TokenKind::Token),
            Symbol::LANGUGAE => self.emit(TokenKind::Language),
            Symbol::PARAMETERS => self.emit(TokenKind::Pars),
            Symbol::LIMIT => self.emit(TokenKind::Limit),
            _ => self.emit(TokenKind::Id(sym)),
        }
    }
    fn state_int(&mut self) -> Transition {
        self.accept_star(|c| c.is_ascii_digit());
        if let Ok(val) = self.get(0, 0).parse() {
            self.emit(TokenKind::Int(val))
        } else {
            self.emit_invalid("invalid integer")
        }
    }
    fn state_str(&mut self) -> Transition {
        let mut valid = true;
        while let Some(c) = self.consume() {
            match c {
                '\\' => {
                    if !self.accept_char('\'') {
                        valid = false;
                    }
                }
                '\'' => {
                    if valid {
                        let sym = self.get(1, 1).replace("\\", "").into_symbol();
                        return self.emit(TokenKind::Str(sym));
                    } else {
                        return self.emit_invalid("invalid string literal");
                    }
                }
                '\n' => {
                    self.backup();
                    return self.emit_invalid("invalid string literal");
                }
                _ => (),
            }
        }
        self.emit_invalid("invalid string literal")
    }
    fn state_code(&mut self) -> Transition {
        let mut i = 1;
        loop {
            match self.consume() {
                Some('{') => {
                    i += 1;
                }
                Some('}') => {
                    i -= 1;
                    if i == 0 {
                        break;
                    }
                }
                Some('\n') => {
                    self.line();
                }
                None => return self.emit_invalid("unclosed code segment"),
                _ => (),
            }
        }
        let sym = self.get(1, 1).into_symbol();
        self.emit(TokenKind::Code(sym))
    }
    fn state_pred(&mut self) -> Transition {
        if self.accept_plus(|c| c.is_ascii_digit()) {
            if let Ok(val) = self.get(1, 0).parse() {
                self.emit(TokenKind::Predicate(val))
            } else {
                self.emit_invalid("invalid predicate")
            }
        } else {
            self.emit_invalid("invalid predicate")
        }
    }
    fn state_sema(&mut self) -> Transition {
        if self.accept_plus(|c| c.is_ascii_digit()) {
            if let Ok(val) = self.get(1, 0).parse() {
                self.emit(TokenKind::Action(val))
            } else {
                self.emit_invalid("invalid action")
            }
        } else {
            self.emit_invalid("invalid action")
        }
    }
    fn state_error(&mut self) -> Transition {
        if self.accept_plus(|c| c.is_ascii_digit()) {
            if let Ok(val) = self.get(1, 0).parse() {
                self.emit(TokenKind::ErrorHandler(val))
            } else {
                self.emit_invalid("invalid error handler")
            }
        } else {
            self.emit_invalid("invalid error handler")
        }
    }
    fn state_slash(&mut self) -> Transition {
        match self.consume() {
            Some('/') => self.state_cpp_comment(),
            Some('*') => self.state_c_comment(),
            _ => {
                self.backup();
                self.emit_invalid("invalid token")
            }
        }
    }
    fn state_cpp_comment(&mut self) -> Transition {
        self.accept_star(|c| c != '\n');
        self.consume();
        self.line();
        if let Some('/') = self.get(2, 0).chars().next() {
            if let Some(Token {
                kind: TokenKind::_Comment(prev),
                ..
            }) = self.trivia
            {
                // combine with previous comment
                let sym = format!("{}\n{}", prev, self.get(3, 0).trim()).into_symbol();
                self.emit_trivia(TokenKind::_Comment(sym))
            } else {
                let sym = self.get(3, 0).trim().into_symbol();
                self.emit_trivia(TokenKind::_Comment(sym))
            }
        } else {
            self.trivia = None;
            self.ignore()
        }
    }
    fn state_c_comment(&mut self) -> Transition {
        loop {
            let star = self.accept_plus(|c| c == '*');
            match self.consume() {
                Some('/') if star => break,
                Some('\n') => self.line(),
                Some(_) => (),
                None => return self.emit_invalid("unclosed block comment"),
            }
        }
        self.trivia = None;
        self.ignore()
    }
}
