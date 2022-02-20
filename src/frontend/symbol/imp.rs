use super::*;

macro_rules! predefine {
    ( $([$id:ident, $name:expr]),* $(,)? ) => {
        #[repr(usize)]
        #[allow(clippy::upper_case_acronyms)]
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
    [EOF,        "EOF"],
    [RUST,       "rust"],
}
