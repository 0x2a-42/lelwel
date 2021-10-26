use super::*;
use std::str::FromStr;

impl Lexer {
    pub fn state_start(&mut self) -> Transition {
        match self.consume() {
            Some(c) if c.is_ascii_digit() => self.state_num(),
            Some(c) if c.is_whitespace() => self.state_ws(),
            Some('+') => self.emit(TokenKind::Add),
            Some('-') => self.emit(TokenKind::Sub),
            Some('*') => self.emit(TokenKind::Mul),
            Some('/') => self.emit(TokenKind::Div),
            Some('(') => self.emit(TokenKind::LPar),
            Some(')') => self.emit(TokenKind::RPar),
            None => self.emit(TokenKind::EOF),
            _ => {
                self.error("invalid token");
                self.ignore()
            }
        }
    }
    fn state_ws(&mut self) -> Transition {
        self.accept_star(|c| c.is_whitespace());
        self.ignore()
    }
    fn state_num(&mut self) -> Transition {
        self.accept_star(|c| c.is_ascii_digit());
        if self.accept_oneof(".") {
            self.accept_star(|c| c.is_ascii_digit());
        }
        if let Ok(value) = f64::from_str(self.get(0, 0)) {
            self.emit(TokenKind::Num(value))
        } else {
            self.error("invalid number");
            self.ignore()
        }
    }
}
