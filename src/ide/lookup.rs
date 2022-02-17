use crate::frontend::{ast::*, token::*};

pub struct LookupNode {}

impl<'a> LookupNode {
    pub fn find(module: &'a Module<'a>, pos: Position) -> Option<Node<'a>> {
        Self::visit_element(module.elements.lookup(pos)?, pos)
    }

    fn visit_element(element: &'a Element<'a>, pos: Position) -> Option<Node<'a>> {
        match &element.kind {
            ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                if let Some(regex) = regex.lookup(pos) {
                    Self::visit_regex(regex, pos).map(Node::Regex)
                } else if pos < regex.range().start {
                    Some(Node::Element(element))
                } else {
                    None
                }
            }
            ElementKind::Predicate { .. }
            | ElementKind::Action { .. }
            | ElementKind::ErrorHandler { .. }
            | ElementKind::Token { .. } => Some(Node::Element(element)),
            _ => None,
        }
    }

    fn visit_regex(regex: &'a Regex<'a>, pos: Position) -> Option<&'a Regex<'a>> {
        match &regex.kind {
            RegexKind::Concat { ops, .. } | RegexKind::Or { ops, .. } => {
                if let Some(regex) = ops.lookup(pos) {
                    Self::visit_regex(regex, pos)
                } else {
                    Some(regex)
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                if let Some(regex) = op.lookup(pos) {
                    Self::visit_regex(regex, pos)
                } else {
                    Some(regex)
                }
            }
            _ => Some(regex),
        }
    }
}

pub struct LookupDefinition {}

impl<'a> LookupDefinition {
    pub fn find(module: &'a Module<'a>, pos: Position) -> Option<&'a Element<'a>> {
        match LookupNode::find(module, pos) {
            Some(Node::Element(element)) => Some(element),
            Some(Node::Regex(Regex {
                kind:
                    RegexKind::Id { elem, .. }
                    | RegexKind::Str { elem, .. }
                    | RegexKind::Predicate { elem, .. }
                    | RegexKind::Action { elem, .. }
                    | RegexKind::ErrorHandler { elem, .. },
                ..
            })) => elem.get(),
            _ => None,
        }
    }
}

pub struct LookupReferences {}

impl<'a> LookupReferences {
    pub fn find(module: &'a Module<'a>, pos: Position, with_def: bool) -> Vec<Range> {
        let mut refs = vec![];
        if let Some(def) = LookupDefinition::find(module, pos) {
            if with_def {
                refs.push(def.range());
            }
            for element in module.elements.iter() {
                Self::visit_element(element, def, &mut refs);
            }
        }
        refs
    }

    fn visit_element(element: &'a Element<'a>, def: &'a Element<'a>, refs: &mut Vec<Range>) {
        match &element.kind {
            ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                Self::visit_regex(regex, def, refs)
            }
            _ => {}
        }
    }

    fn visit_regex(regex: &'a Regex<'a>, def: &'a Element<'a>, refs: &mut Vec<Range>) {
        match &regex.kind {
            RegexKind::Concat { ops, .. } | RegexKind::Or { ops, .. } => {
                for op in ops {
                    Self::visit_regex(op, def, refs)
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => Self::visit_regex(op, def, refs),
            RegexKind::Id { elem, .. }
            | RegexKind::Str { elem, .. }
            | RegexKind::Predicate { elem, .. }
            | RegexKind::Action { elem, .. }
            | RegexKind::ErrorHandler { elem, .. } => {
                if let Some(elem) = elem.get() {
                    if std::ptr::eq(elem, def) {
                        refs.push(regex.range());
                    }
                }
            }
            _ => {}
        }
    }
}
