mod imp;

pub use imp::*;

use super::diag::*;
use super::parser::*;
use super::token::*;
use bumpalo::Bump;
use std::cell::Cell;

/// Abstract syntax tree

#[derive(Debug)]
pub struct Ast<'a, Input: TokenStream> {
    arena: Bump,
    root: Option<&'a <Parser as Parsing<'a, Input>>::Output>,
}

impl<'a, Input: TokenStream> Ast<'a, Input> {
    /// Creates a new abstract syntax tree.
    pub fn new(input: &mut Input, diag: &mut Diag) -> Ast<'a, Input> {
        let mut ast = Ast {
            arena: Bump::new(),
            root: None,
        };
        // borrow arena for the lifetime of the returned Ast
        match Parser::parse(input, Self::extend(&ast.arena), diag) {
            Ok(root) => ast.root = Some(Self::extend(ast.arena.alloc(root))),
            Err(e) => diag.error(e, input.current().range),
        }
        ast
    }

    /// Gets the root node of the `Ast`.
    #[allow(dead_code)]
    pub fn root(&self) -> Option<&'a <Parser as Parsing<'a, Input>>::Output> {
        self.root
    }

    /// Extends lifetime of reference to lifetime of `Ast`.
    #[allow(dead_code)]
    fn extend<'b, T>(reference: &'b T) -> &'a T {
        unsafe { &*(reference as *const T) }
    }
}

/// Reference to another node in the `Ast`.
pub struct Ref<'a, T> {
    target: Cell<Option<&'a T>>,
}

#[allow(dead_code)]
impl<'a, T> Ref<'a, T> {
    /// Creates a new `Ref`.
    pub fn new(init: Option<&'a T>) -> Ref<'a, T> {
        Ref {
            target: Cell::new(init),
        }
    }

    /// Sets the target of the `Ref`.
    pub fn set(&self, value: &'a T) {
        self.target.set(Some(value));
    }

    /// Gets the target of the `Ref`.
    pub fn get(&self) -> Option<&'a T> {
        self.target.get()
    }
}

impl<'a, T> std::fmt::Debug for Ref<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = match self.get() {
            Some(e) => e,
            None => std::ptr::null(),
        };
        std::fmt::Pointer::fmt(&ptr, f)
    }
}

impl<'a, T> std::fmt::Pointer for Ref<'a, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = match self.get() {
            Some(e) => e,
            None => std::ptr::null(),
        };
        std::fmt::Pointer::fmt(&ptr, f)
    }
}
