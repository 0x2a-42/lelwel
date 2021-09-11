use super::super::symbol::Symbol;
use super::*;
use bumpalo::{boxed::Box, collections::Vec as BVec, Bump};
use std::cell::RefCell;
use std::collections::BTreeSet;

#[derive(Debug)]
pub struct Module<'a> {
    pub elements: BVec<'a, &'a Element<'a>>,
    pub preamble: Cell<Option<&'a Element<'a>>>,
    pub parameters: Cell<Option<&'a Element<'a>>>,
    pub arguments: Cell<Option<&'a Element<'a>>>,
    pub error: Cell<Option<&'a Element<'a>>>,
    pub limit: Cell<Option<&'a Element<'a>>>,
    pub language: Cell<Option<&'a Element<'a>>>,
}

impl<'a> Module<'a> {
    pub fn new(elements: BVec<'a, &'a Element<'a>>) -> Module<'a> {
        Module {
            elements,
            preamble: Cell::new(None),
            parameters: Cell::new(None),
            arguments: Cell::new(None),
            error: Cell::new(None),
            limit: Cell::new(None),
            language: Cell::new(None),
        }
    }
}

#[derive(Debug)]
pub struct ElementAttr {
    range: Range,
    pub used: Cell<bool>,
    doc: Symbol,
}

impl ElementAttr {
    pub fn new(range: Range) -> ElementAttr {
        ElementAttr {
            range,
            used: Cell::new(false),
            doc: Symbol::EMPTY,
        }
    }
    pub fn new_with_doc(range: Range, doc: Option<Token>) -> ElementAttr {
        let doc = if let Some(Token {
            kind: TokenKind::_Comment(sym),
            ..
        }) = doc
        {
            sym
        } else {
            Symbol::EMPTY
        };
        ElementAttr {
            range,
            used: Cell::new(false),
            doc,
        }
    }
    pub fn doc(&self) -> Symbol {
        self.doc
    }
}

#[derive(Debug)]
pub struct RegexAttr<'a> {
    /// Range of the regex.
    range: Range,
    /// First set of the regex (use BTreeSet so order in genereted code is fixed).
    first: Box<'a, RefCell<BTreeSet<Symbol>>>,
    /// Follow set of the regex (use BTreeSet so order in genereted code is fixed).
    follow: Box<'a, RefCell<BTreeSet<Symbol>>>,
}

impl<'a> RegexAttr<'a> {
    pub fn new_in(arena: &'a Bump, range: Range) -> RegexAttr<'a> {
        RegexAttr {
            range,
            first: Box::new_in(RefCell::new(BTreeSet::new()), arena),
            follow: Box::new_in(RefCell::new(BTreeSet::new()), arena),
        }
    }
}

#[derive(Debug)]
pub enum Node<'a> {
    Element(&'a Element<'a>),
    Regex(&'a Regex<'a>),
}

#[derive(Debug)]
pub struct Element<'a> {
    pub kind: ElementKind<'a>,
    pub attr: ElementAttr,
}

#[derive(Debug)]
pub enum ElementKind<'a> {
    Start {
        ret: Symbol,
        pars: Symbol,
        regex: &'a Regex<'a>,
        action: Ref<'a, Element<'a>>,
    },
    Rule {
        name: Symbol,
        ret: Symbol,
        pars: Symbol,
        regex: &'a Regex<'a>,
        action: Ref<'a, Element<'a>>,
    },
    Token {
        name: Symbol,
        ty: Symbol,
        sym: Symbol,
    },
    Action {
        name: Symbol,
        num: u64,
        code: Symbol,
    },
    Predicate {
        name: Symbol,
        num: u64,
        code: Symbol,
    },
    ErrorHandler {
        name: Symbol,
        num: u64,
        code: Symbol,
    },
    Preamble {
        code: Symbol,
    },
    Parameters {
        code: Symbol,
    },
    ErrorCode {
        code: Symbol,
    },
    Language {
        name: Symbol,
    },
    Limit {
        depth: u16,
    },
    Invalid,
}

impl<'a> Element<'a> {
    pub fn new_start(
        arena: &'a Bump,
        ret: Symbol,
        pars: Symbol,
        regex: &'a Regex<'a>,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Start {
                ret,
                pars,
                regex,
                action: Ref::new(None),
            },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_rule(
        arena: &'a Bump,
        name: Symbol,
        ret: Symbol,
        pars: Symbol,
        regex: &'a Regex<'a>,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Rule {
                name,
                ret,
                pars,
                regex,
                action: Ref::new(None),
            },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_token(
        arena: &'a Bump,
        name: Symbol,
        ty: Symbol,
        sym: Symbol,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Token { name, ty, sym },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_action(
        arena: &'a Bump,
        name: Symbol,
        num: u64,
        code: Symbol,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Action { name, num, code },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_predicate(
        arena: &'a Bump,
        name: Symbol,
        num: u64,
        code: Symbol,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Predicate { name, num, code },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_error_handler(
        arena: &'a Bump,
        name: Symbol,
        num: u64,
        code: Symbol,
        range: Range,
        doc: Option<Token>,
    ) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::ErrorHandler { name, num, code },
            attr: ElementAttr::new_with_doc(range, doc),
        })
    }
    pub fn new_preamble(arena: &'a Bump, code: Symbol, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Preamble { code },
            attr: ElementAttr::new(range),
        })
    }
    pub fn new_parameters(arena: &'a Bump, code: Symbol, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Parameters { code },
            attr: ElementAttr::new(range),
        })
    }
    pub fn new_error_code(arena: &'a Bump, code: Symbol, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::ErrorCode { code },
            attr: ElementAttr::new(range),
        })
    }
    pub fn new_language(arena: &'a Bump, name: Symbol, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Language { name },
            attr: ElementAttr::new(range),
        })
    }
    pub fn new_limit(arena: &'a Bump, depth: u16, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Limit { depth },
            attr: ElementAttr::new(range),
        })
    }
    pub fn new_invalid(arena: &'a Bump, range: Range) -> &'a Element<'a> {
        arena.alloc(Element {
            kind: ElementKind::Invalid,
            attr: ElementAttr::new(range),
        })
    }
}

#[derive(Debug)]
pub struct Regex<'a> {
    pub kind: RegexKind<'a>,
    pub attr: RegexAttr<'a>,
}

#[derive(Debug)]
pub enum RegexKind<'a> {
    Id {
        name: Symbol,
        elem: Ref<'a, Element<'a>>,
    },
    Concat {
        ops: BVec<'a, &'a Regex<'a>>,
        error: Ref<'a, Regex<'a>>,
    },
    Or {
        ops: BVec<'a, &'a Regex<'a>>,
        error: Ref<'a, Regex<'a>>,
    },
    Star {
        op: &'a Regex<'a>,
    },
    Plus {
        op: &'a Regex<'a>,
    },
    Option {
        op: &'a Regex<'a>,
    },
    Paren {
        op: &'a Regex<'a>,
    },
    Str {
        val: Symbol,
        elem: Ref<'a, Element<'a>>,
    },
    Predicate {
        val: u64,
        elem: Ref<'a, Element<'a>>,
    },
    Action {
        val: u64,
        elem: Ref<'a, Element<'a>>,
    },
    ErrorHandler {
        val: u64,
        elem: Ref<'a, Element<'a>>,
    },
    Empty,
    Invalid,
}

impl<'a> Regex<'a> {
    pub fn new_id(arena: &'a Bump, name: Symbol, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Id {
                name,
                elem: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_concat(
        arena: &'a Bump,
        ops: BVec<'a, &'a Regex<'a>>,
        range: Range,
    ) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Concat {
                ops,
                error: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_or(arena: &'a Bump, ops: BVec<'a, &'a Regex<'a>>, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Or {
                ops,
                error: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_star(arena: &'a Bump, op: &'a Regex<'a>, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Star { op },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_plus(arena: &'a Bump, op: &'a Regex<'a>, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Plus { op },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_option(arena: &'a Bump, op: &'a Regex<'a>, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Option { op },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_paren(arena: &'a Bump, op: &'a Regex<'a>, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Paren { op },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_str(arena: &'a Bump, val: Symbol, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Str {
                val,
                elem: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_predicate(arena: &'a Bump, val: u64, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Predicate {
                val,
                elem: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_action(arena: &'a Bump, val: u64, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Action {
                val,
                elem: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_error_handler(arena: &'a Bump, val: u64, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::ErrorHandler {
                val,
                elem: Ref::new(None),
            },
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_empty(arena: &'a Bump, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Empty,
            attr: RegexAttr::new_in(arena, range),
        })
    }
    pub fn new_invalid(arena: &'a Bump, range: Range) -> &'a Regex<'a> {
        arena.alloc(Regex {
            kind: RegexKind::Invalid,
            attr: RegexAttr::new_in(arena, range),
        })
    }

    pub fn first(&self) -> std::cell::Ref<BTreeSet<Symbol>> {
        self.attr.first.borrow()
    }
    pub fn first_mut(&self) -> std::cell::RefMut<BTreeSet<Symbol>> {
        self.attr.first.borrow_mut()
    }
    pub fn follow(&self) -> std::cell::Ref<BTreeSet<Symbol>> {
        self.attr.follow.borrow()
    }
    pub fn follow_mut(&self) -> std::cell::RefMut<BTreeSet<Symbol>> {
        self.attr.follow.borrow_mut()
    }
    pub fn predict(&self) -> BTreeSet<Symbol> {
        let mut set = self.first().clone();
        if set.contains(&Symbol::EMPTY) {
            set.remove(&Symbol::EMPTY);
            set.extend(self.follow().iter());
        }
        set
    }
}

impl<'a> Ranged for Node<'a> {
    fn range(&self) -> Range {
        match self {
            Node::Element(element) => element.range(),
            Node::Regex(regex) => regex.range(),
        }
    }
}

impl<'a> Ranged for Element<'a> {
    fn range(&self) -> Range {
        self.attr.range
    }
}

impl<'a> Ranged for Regex<'a> {
    fn range(&self) -> Range {
        self.attr.range
    }
}
