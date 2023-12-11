use crate::frontend::ast::*;

fn compare(span: &Span, pos: usize) -> std::cmp::Ordering {
    if span.start > pos {
        std::cmp::Ordering::Greater
    } else if span.end <= pos {
        std::cmp::Ordering::Less
    } else {
        std::cmp::Ordering::Equal
    }
}

pub struct LookupNode {}

impl<'a> LookupNode {
    pub fn find(module: &'a Module<'a>, pos: usize) -> Option<Node<'a>> {
        let index = module
            .elements
            .binary_search_by(|elem| compare(&elem.span, pos))
            .ok()?;
        Self::visit_element(module, &module.elements[index], pos)
    }

    fn visit_element(
        module: &'a Module<'a>,
        element: &'a Element<'a>,
        pos: usize,
    ) -> Option<Node<'a>> {
        match element.kind {
            ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                let regex = module.get_regex(regex).unwrap();
                if regex.span.contains(&pos) {
                    Self::visit_regex(module, regex, pos).map(Node::Regex)
                } else if pos < regex.span.start {
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

    fn visit_regex(
        module: &'a Module<'a>,
        regex: &'a Regex<'a>,
        pos: usize,
    ) -> Option<&'a Regex<'a>> {
        match regex.kind {
            RegexKind::Concat { ref ops, .. } | RegexKind::Or { ref ops, .. } => {
                if let Ok(index) = ops
                    .binary_search_by(|regex| compare(&module.get_regex(*regex).unwrap().span, pos))
                {
                    Self::visit_regex(module, module.get_regex(ops[index]).unwrap(), pos)
                } else {
                    Some(regex)
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                let op = module.get_regex(op).unwrap();
                if op.span.contains(&pos) {
                    Self::visit_regex(module, op, pos)
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
    pub fn find(module: &'a Module<'a>, pos: usize) -> Option<&'a Element<'a>> {
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
            })) => module.get_element(*elem),
            _ => None,
        }
    }
}

pub struct LookupReferences {}

impl<'a> LookupReferences {
    pub fn find(module: &'a Module<'a>, pos: usize, with_def: bool) -> Vec<Span> {
        let mut refs = vec![];
        if let Some(def) = LookupDefinition::find(module, pos) {
            if with_def {
                refs.push(def.span.clone());
            }
            for element in module.elements.iter() {
                Self::visit_element(module, element, def, &mut refs);
            }
        }
        refs
    }

    fn visit_element(
        module: &'a Module<'a>,
        element: &'a Element<'a>,
        def: &'a Element<'a>,
        refs: &mut Vec<Span>,
    ) {
        match element.kind {
            ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                Self::visit_regex(module, module.get_regex(regex).unwrap(), def, refs)
            }
            _ => {}
        }
    }

    fn visit_regex(
        module: &'a Module<'a>,
        regex: &'a Regex<'a>,
        def: &'a Element<'a>,
        refs: &mut Vec<Span>,
    ) {
        match regex.kind {
            RegexKind::Concat { ref ops, .. } | RegexKind::Or { ref ops, .. } => {
                for op in ops {
                    Self::visit_regex(module, module.get_regex(*op).unwrap(), def, refs)
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                Self::visit_regex(module, module.get_regex(op).unwrap(), def, refs)
            }
            RegexKind::Id { elem, .. }
            | RegexKind::Str { elem, .. }
            | RegexKind::Predicate { elem, .. }
            | RegexKind::Action { elem, .. }
            | RegexKind::ErrorHandler { elem, .. } => {
                if let Some(elem) = module.get_element(elem) {
                    if std::ptr::eq(elem, def) {
                        refs.push(regex.span.clone());
                    }
                }
            }
            _ => {}
        }
    }
}
