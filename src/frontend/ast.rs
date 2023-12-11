use super::symbols;
use super::symbols::Symbol;
use std::collections::BTreeSet;

pub type Span = core::ops::Range<usize>;

/// Abstract syntax tree

#[derive(Debug)]
pub struct Module<'a> {
    pub uri: &'a str,
    pub elements: Vec<Element<'a>>,
    pub regexes: Vec<Regex<'a>>,

    pub parameters: ElementRef,
}

impl<'a> Module<'a> {
    pub fn new(uri: &'a str) -> Self {
        Module {
            uri,
            elements: vec![],
            regexes: vec![],
            parameters: ElementRef::INVALID,
        }
    }

    fn alloc_element(&mut self, kind: ElementKind<'a>, span: Span, doc: &'a str) -> ElementRef {
        let index = self.elements.len();
        self.elements.push(Element {
            kind,
            used: false,
            span,
            doc,
        });
        ElementRef(index)
    }
    fn alloc_regex(&mut self, kind: RegexKind<'a>, span: Span) -> RegexRef {
        let index = self.regexes.len();
        self.regexes.push(Regex {
            kind,
            span,
            first: BTreeSet::new(),
            follow: BTreeSet::new(),
            cancel: BTreeSet::new(),
        });
        RegexRef(index)
    }

    pub fn get_element(&self, r: ElementRef) -> Option<&Element<'a>> {
        self.elements.get(r.0)
    }
    pub fn get_regex(&self, r: RegexRef) -> Option<&Regex<'a>> {
        self.regexes.get(r.0)
    }
    pub fn get_element_mut(&mut self, r: ElementRef) -> &mut Element<'a> {
        &mut self.elements[r.0]
    }
    pub fn get_regex_mut(&mut self, r: RegexRef) -> &mut Regex<'a> {
        &mut self.regexes[r.0]
    }
}

#[derive(Debug)]
pub enum Node<'a> {
    Element(&'a Element<'a>),
    Regex(&'a Regex<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ElementRef(pub usize);

impl ElementRef {
    pub const INVALID: Self = Self(usize::MAX);

    pub fn is_valid(self) -> bool {
        self != Self::INVALID
    }
}
impl From<usize> for ElementRef {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct Element<'a> {
    pub kind: ElementKind<'a>,
    pub used: bool,
    pub span: Span,
    pub doc: &'a str,
}

#[derive(Debug)]
pub enum ElementKind<'a> {
    Start {
        ret: &'a str,
        pars: &'a str,
        regex: RegexRef,
        action: ElementRef,
    },
    Rule {
        name: Symbol<'a>,
        name_span: Span,
        ret: &'a str,
        pars: &'a str,
        regex: RegexRef,
        action: ElementRef,
    },
    Token {
        name: Symbol<'a>,
        ty: &'a str,
        sym: Symbol<'a>,
    },
    Action {
        name: Symbol<'a>,
        name_span: Span,
        num: u64,
        code: &'a str,
    },
    Predicate {
        name: Symbol<'a>,
        name_span: Span,
        num: u64,
        code: &'a str,
    },
    ErrorHandler {
        name: Symbol<'a>,
        name_span: Span,
        num: u64,
        code: &'a str,
    },
    Parameters {
        code: &'a str,
    },
    Invalid,
}

impl<'a> Element<'a> {
    pub fn new_start(
        module: &mut Module<'a>,
        ret: &'a str,
        pars: &'a str,
        regex: RegexRef,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(
            ElementKind::Start {
                ret,
                pars,
                regex,
                action: ElementRef::INVALID,
            },
            span.clone(),
            doc,
        )
    }
    #[allow(clippy::too_many_arguments)]
    pub fn new_rule(
        module: &mut Module<'a>,
        name: Symbol<'a>,
        name_span: Span,
        ret: &'a str,
        pars: &'a str,
        regex: RegexRef,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(
            ElementKind::Rule {
                name,
                name_span,
                ret,
                pars,
                regex,
                action: ElementRef::INVALID,
            },
            span.clone(),
            doc,
        )
    }
    pub fn new_token(
        module: &mut Module<'a>,
        name: Symbol<'a>,
        ty: &'a str,
        sym: Symbol<'a>,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(ElementKind::Token { name, ty, sym }, span.clone(), doc)
    }
    pub fn new_action(
        module: &mut Module<'a>,
        name: Symbol<'a>,
        name_span: &Span,
        num: u64,
        code: &'a str,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(
            ElementKind::Action {
                name,
                name_span: name_span.clone(),
                num,
                code,
            },
            span.clone(),
            doc,
        )
    }
    pub fn new_predicate(
        module: &mut Module<'a>,
        name: Symbol<'a>,
        name_span: &Span,
        num: u64,
        code: &'a str,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(
            ElementKind::Predicate {
                name,
                name_span: name_span.clone(),
                num,
                code,
            },
            span.clone(),
            doc,
        )
    }
    pub fn new_error_handler(
        module: &mut Module<'a>,
        name: Symbol<'a>,
        name_span: &Span,
        num: u64,
        code: &'a str,
        span: &Span,
        doc: &'a str,
    ) -> ElementRef {
        module.alloc_element(
            ElementKind::ErrorHandler {
                name,
                name_span: name_span.clone(),
                num,
                code,
            },
            span.clone(),
            doc,
        )
    }
    pub fn new_parameters(module: &mut Module<'a>, code: &'a str, span: &Span) -> ElementRef {
        module.alloc_element(ElementKind::Parameters { code }, span.clone(), "")
    }
    pub fn new_invalid(module: &mut Module<'a>, span: &Span) -> ElementRef {
        module.alloc_element(ElementKind::Invalid, span.clone(), "")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RegexRef(pub usize);

impl RegexRef {
    pub const INVALID: Self = Self(usize::MAX);

    pub fn is_valid(self) -> bool {
        self != Self::INVALID
    }
}
impl From<usize> for RegexRef {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Debug)]
pub struct Regex<'a> {
    pub kind: RegexKind<'a>,
    pub span: Span,
    pub first: BTreeSet<Symbol<'a>>,
    pub follow: BTreeSet<Symbol<'a>>,
    pub cancel: BTreeSet<Symbol<'a>>,
}

#[derive(Debug)]
pub enum RegexKind<'a> {
    Id {
        name: Symbol<'a>,
        elem: ElementRef,
    },
    Concat {
        ops: Vec<RegexRef>,
        error: RegexRef,
    },
    Or {
        ops: Vec<RegexRef>,
        error: RegexRef,
    },
    Star {
        op: RegexRef,
    },
    Plus {
        op: RegexRef,
    },
    Option {
        op: RegexRef,
    },
    Paren {
        op: RegexRef,
    },
    Str {
        val: Symbol<'a>,
        elem: ElementRef,
    },
    Predicate {
        rule_name: Symbol<'a>,
        val: u64,
        elem: ElementRef,
    },
    Action {
        rule_name: Symbol<'a>,
        val: u64,
        elem: ElementRef,
    },
    ErrorHandler {
        rule_name: Symbol<'a>,
        val: u64,
        elem: ElementRef,
    },
    Empty,
    Invalid,
}

impl<'a> Regex<'a> {
    pub fn new_id(module: &mut Module<'a>, name: Symbol<'a>, span: &Span) -> RegexRef {
        module.alloc_regex(
            RegexKind::Id {
                name,
                elem: ElementRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_concat(module: &mut Module<'a>, ops: Vec<RegexRef>, span: &Span) -> RegexRef {
        module.alloc_regex(
            RegexKind::Concat {
                ops,
                error: RegexRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_or(module: &mut Module<'a>, ops: Vec<RegexRef>, span: &Span) -> RegexRef {
        module.alloc_regex(
            RegexKind::Or {
                ops,
                error: RegexRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_star(module: &mut Module<'a>, op: RegexRef, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Star { op }, span.clone())
    }
    pub fn new_plus(module: &mut Module<'a>, op: RegexRef, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Plus { op }, span.clone())
    }
    pub fn new_option(module: &mut Module<'a>, op: RegexRef, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Option { op }, span.clone())
    }
    pub fn new_paren(module: &mut Module<'a>, op: RegexRef, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Paren { op }, span.clone())
    }
    pub fn new_str(module: &mut Module<'a>, val: Symbol<'a>, span: &Span) -> RegexRef {
        module.alloc_regex(
            RegexKind::Str {
                val,
                elem: ElementRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_predicate(
        module: &mut Module<'a>,
        rule_name: Symbol<'a>,
        val: u64,
        span: &Span,
    ) -> RegexRef {
        module.alloc_regex(
            RegexKind::Predicate {
                rule_name,
                val,
                elem: ElementRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_action(
        module: &mut Module<'a>,
        rule_name: Symbol<'a>,
        val: u64,
        span: &Span,
    ) -> RegexRef {
        module.alloc_regex(
            RegexKind::Action {
                rule_name,
                val,
                elem: ElementRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_error_handler(
        module: &mut Module<'a>,
        rule_name: Symbol<'a>,
        val: u64,
        span: &Span,
    ) -> RegexRef {
        module.alloc_regex(
            RegexKind::ErrorHandler {
                rule_name,
                val,
                elem: ElementRef::INVALID,
            },
            span.clone(),
        )
    }
    pub fn new_empty(module: &mut Module<'a>, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Empty, span.clone())
    }
    pub fn new_invalid(module: &mut Module<'a>, span: &Span) -> RegexRef {
        module.alloc_regex(RegexKind::Invalid, span.clone())
    }

    pub fn predict(&self) -> BTreeSet<Symbol> {
        let mut set = self.first.clone();
        if set.contains(&symbols::EMPTY) {
            set.remove(&symbols::EMPTY);
            set.extend(self.follow.iter());
        }
        set
    }
}
