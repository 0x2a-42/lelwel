use super::*;

trait JsonChars {
    fn is_json_whitespace(self) -> bool;
    fn is_json_character(self) -> bool;
    fn is_json_delimiter(self) -> bool;
}

impl JsonChars for char {
    fn is_json_whitespace(self) -> bool {
        self == '\u{0020}' || self == '\u{000A}' || self == '\u{000D}' || self == '\u{0009}'
    }
    fn is_json_character(self) -> bool {
        '\u{0020}' <= self && self <= '\u{10FFFF}'
    }
    fn is_json_delimiter(self) -> bool {
        self.is_json_whitespace()
            || self == ','
            || self == ']'
            || self == '}'
            || self == '['
            || self == '{'
            || self == ':'
            || self == '"'
    }
}

impl Lexer {
    pub fn state_start(&mut self) -> Transition {
        match self.consume() {
            Some(c) if c.is_ascii_alphabetic() => self.state_identifier(),
            Some(c) if c.is_ascii_digit() => self.state_number(false),
            Some(c) if c.is_json_whitespace() => self.state_ws(c),
            Some('-') => self.state_number(true),
            Some('"') => self.state_string(),
            Some('{') => self.emit(TokenKind::LBrace),
            Some('}') => self.emit(TokenKind::RBrace),
            Some('[') => self.emit(TokenKind::LBrak),
            Some(']') => self.emit(TokenKind::RBrak),
            Some(',') => self.emit(TokenKind::Comma),
            Some(':') => self.emit(TokenKind::Colon),
            None => self.emit(TokenKind::EOF),
            _ => {
                self.accept_star(|c| c.is_json_delimiter());
                self.emit_invalid()
            }
        }
    }
    fn state_ws(&mut self, c: char) -> Transition {
        if c == '\n' {
            self.line();
        }
        loop {
            match self.consume() {
                Some('\n') => self.line(),
                Some(c) if c.is_json_whitespace() => (),
                None => break,
                _ => {
                    self.backup();
                    break;
                }
            }
        }
        self.ignore()
    }
    fn state_identifier(&mut self) -> Transition {
        self.accept_star(|c| c.is_ascii_alphabetic());
        match self.get(0, 0) {
            "true" => self.emit(TokenKind::True),
            "false" => self.emit(TokenKind::False),
            "null" => self.emit(TokenKind::Null),
            _ => self.emit_invalid(),
        }
    }
    fn state_string(&mut self) -> Transition {
        let mut is_valid = true;
        loop {
            match self.consume() {
                None => break self.emit_invalid(),
                Some('"') => {
                    let val = self.get(1, 1).to_string();
                    break if is_valid {
                        self.emit(TokenKind::String(val))
                    } else {
                        self.emit_invalid()
                    };
                }
                Some('\\') => match self.consume() {
                    Some('"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't') => {}
                    Some('u') => {
                        if !self.accept_count(|c| c.is_ascii_hexdigit(), 4) {
                            is_valid = false;
                        }
                    }
                    _ => is_valid = false,
                },
                Some(c) if c.is_json_character() => {}
                _ => is_valid = false,
            }
        }
    }
    fn state_number(&mut self, has_sign: bool) -> Transition {
        let mut is_valid = true;
        if has_sign {
            if !self.accept(|c| c.is_ascii_digit()) {
                return self.emit_invalid();
            }
        } else {
            self.accept_char('-');
        }
        match self.get_char() {
            Some('0') => {}
            Some('1'..='9') => {
                self.accept_star(|c| c.is_ascii_digit());
            }
            _ => {
                is_valid = false;
            }
        }
        if self.accept_char('.') && !self.accept_plus(|c| c.is_ascii_digit()) {
            is_valid = false;
        }
        if self.accept_oneof("eE") {
            self.accept_oneof("-+");
            if !self.accept_plus(|c| c.is_ascii_digit()) {
                is_valid = false;
            }
        }
        if self.accept_plus(|c| !c.is_json_delimiter()) {
            is_valid = false;
        }
        if is_valid {
            let val = self.get(0, 0).to_string();
            self.emit(TokenKind::Number(val))
        } else {
            self.emit_invalid()
        }
    }
}
