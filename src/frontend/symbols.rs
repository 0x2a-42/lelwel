use std::collections::HashMap;

#[derive(Clone, Copy, Ord, PartialOrd, Eq)]
pub struct Symbol<'a>(pub &'a str);

pub static EMPTY: Symbol = Symbol("ɛ");
pub static EOF: Symbol = Symbol("EOF");
pub static START: Symbol = Symbol("start");

impl<'a> Default for Symbol<'a> {
    fn default() -> Self {
        EMPTY
    }
}
impl<'a> core::fmt::Debug for Symbol<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl<'a> PartialEq for Symbol<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}
impl<'a> std::fmt::Display for Symbol<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}
impl<'a> std::hash::Hash for Symbol<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state)
    }
}

pub struct StringInterner<'a> {
    map: HashMap<&'a str, Symbol<'a>>,
}
impl<'a> Default for StringInterner<'a> {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> StringInterner<'a> {
    pub fn new() -> Self {
        let mut map = HashMap::new();
        map.insert(EMPTY.0, EMPTY);
        map.insert(EOF.0, EOF);
        map.insert(START.0, START);

        Self { map }
    }
    pub fn get(&mut self, symbol: &'a str) -> Symbol<'a> {
        if !self.map.contains_key(symbol) {
            self.map.insert(symbol, Symbol(symbol));
            Symbol(symbol)
        } else {
            self.map[symbol]
        }
    }
}
