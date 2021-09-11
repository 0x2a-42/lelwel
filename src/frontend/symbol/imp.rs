use super::*;

macro_rules! predefine {
    ( $([$id:ident, $name:expr]),* $(,)? ) => {
        #[repr(usize)]
        enum Predef {
            EMPTY,
            $($id),*
        }
        impl Symbol {
            pub const EMPTY: Symbol = Symbol(Predef::EMPTY as u32);
            $(
            pub const $id: Symbol = Symbol(Predef::$id as u32);
            )*
        }
        impl StringTable {
            pub fn init(&mut self) {
                self.alloc("");
                $(self.alloc($name);)*
            }
        }
    }
}

predefine! {
    [START,      "start"],
    [PREAMBLE,   "preamble"],
    [ERROR,      "error"],
    [TOKEN,      "token"],
    [LANGUGAE,   "language"],
    [PARAMETERS, "parameters"],
    [EOF,        "EOF"],
    [INVALID,    "Invalid"],
    [RUST,       "rust"],
    [LIMIT,      "limit"],
}
