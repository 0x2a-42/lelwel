use codespan_reporting::diagnostic::Severity;
use std::collections::{BTreeSet, HashMap, HashSet};

use super::ast::*;
use super::diag::LanguageErrors;
use super::parser::*;
use super::symbols;
use super::symbols::Symbol;

pub struct SemanticPass {}

impl SemanticPass {
    pub fn run(module: &mut Module, diags: &mut Vec<Diagnostic>) {
        GeneralValidator::run(module, diags);
        if diags.iter().any(|d| d.severity == Severity::Error) {
            return;
        }
        UsageValidator::run(module, diags);
        LL1Validator::run(module, diags);
        CancelSetGenerator::new().run(module);
    }
}

/// general validation of the AST
struct GeneralValidator;

#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
pub enum Binding<'a> {
    Term(Symbol<'a>),
    Token(Symbol<'a>),
    Action(Symbol<'a>, u64),
    Predicate(Symbol<'a>, u64),
    ErrorHandler(Symbol<'a>, u64),
    Parameters,
}

impl<'a> Symbol<'a> {
    fn get_element_kind(self) -> &'static str {
        if self.0.starts_with(|c: char| c.is_uppercase()) {
            "token"
        } else {
            "rule"
        }
    }
}

impl<'a> std::fmt::Display for Binding<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Term(name) => write!(f, "{} `{}`", name.get_element_kind(), name),
            Self::Token(name) => write!(f, "token string `{}`", name),
            Self::Action(name, num) => write!(f, "action `{}#{}`", name, num),
            Self::Predicate(name, num) => write!(f, "predicate `{}?{}`", name, num),
            Self::ErrorHandler(name, num) => write!(f, "error handler `{}!{}`", name, num),
            Self::Parameters => write!(f, "parameters section"),
        }
    }
}

impl<'a> GeneralValidator {
    fn run(module: &mut Module<'a>, diags: &mut Vec<Diagnostic>) {
        // bind rule names to corresponding elements
        Self::bind_elements(module, diags);

        // check elements
        for index in 0..module.elements.len() {
            let element_ref = ElementRef::from(index);
            Self::check_element(module, element_ref, diags);
        }
    }

    /// maps error handlers to their associated regex
    fn map_error_handlers(
        module: &Module<'a>,
        regex: RegexRef,
        diags: &mut Vec<Diagnostic>,
        map: &mut Vec<RegexRef>,
    ) -> Option<RegexRef> {
        let obj = module.get_regex(regex).unwrap();
        match obj.kind {
            RegexKind::Concat { ref ops, .. } => {
                if ops.len() == 1 {
                    Self::map_error_handlers(module, ops[0], diags, map)
                } else {
                    let mut found = None;
                    for (i, op) in ops.iter().enumerate() {
                        found = Self::map_error_handlers(module, *op, diags, map);
                        if found.is_some() && i != ops.len() - 1 {
                            // only allow error handler as last term in concatenation
                            diags.push(Diagnostic::invalid_error_handler_pos(
                                &module.get_regex(*op).unwrap().span,
                                &module.get_regex(*ops.last().unwrap()).unwrap().span,
                            ));
                        }
                    }
                    if let Some(found) = found {
                        map[regex.0] = found;
                    }
                    None
                }
            }
            RegexKind::Or { ref ops, .. } => {
                let mut found = None;
                for op in ops {
                    let last_found = found;
                    found = Self::map_error_handlers(module, *op, diags, map);
                    if let (Some(found), Some(last_found)) = (found, last_found) {
                        // only allow single error handler associated with alternative
                        diags.push(Diagnostic::multiple_error_handler_alt(
                            &module.get_regex(found).unwrap().span,
                            &module.get_regex(last_found).unwrap().span,
                            &obj.span,
                        ));
                    }
                }
                if let Some(found) = found {
                    map[regex.0] = found;
                }
                None
            }
            RegexKind::Star { op } | RegexKind::Plus { op } | RegexKind::Option { op } => {
                Self::map_error_handlers(module, op, diags, map);
                // don't propagate error handler association from here
                None
            }
            RegexKind::Paren { op } => Self::map_error_handlers(module, op, diags, map),
            RegexKind::ErrorHandler { .. } => Some(regex),
            _ => None,
        }
    }

    fn bind_elements(module: &mut Module<'a>, diags: &mut Vec<Diagnostic>) {
        let mut bindings = HashMap::new();
        let mut bind = |binding, element, diags: &mut Vec<Diagnostic>, span| {
            if let Some(old) = bindings.insert(binding, element) {
                let old_span = &module.get_element(old).unwrap().span;
                diags.push(Diagnostic::redefinition(span, binding, old_span));
            }
        };

        let mut error_handler_map = vec![RegexRef::INVALID; module.regexes.len()];

        for (index, element) in module.elements.iter().enumerate() {
            let element_ref = ElementRef::from(index);
            let span = &element.span;
            match element.kind {
                ElementKind::Start { regex, .. } => {
                    bind(Binding::Term(symbols::START), element_ref, diags, span);
                    Self::map_error_handlers(module, regex, diags, &mut error_handler_map);
                }
                ElementKind::Rule { name, regex, .. } => {
                    bind(Binding::Term(name), element_ref, diags, span);
                    Self::map_error_handlers(module, regex, diags, &mut error_handler_map);
                }
                ElementKind::Token { name, sym, .. } => {
                    if name == symbols::EOF {
                        diags.push(Diagnostic::predefined_token_name(span));
                        continue;
                    }
                    bind(Binding::Term(name), element_ref, diags, span);
                    if sym != symbols::EMPTY {
                        bind(Binding::Token(sym), element_ref, diags, span);
                    }
                }
                ElementKind::Action { name, num, .. } => {
                    bind(Binding::Action(name, num), element_ref, diags, span);
                }
                ElementKind::Predicate { name, num, .. } => {
                    bind(Binding::Predicate(name, num), element_ref, diags, span);
                }
                ElementKind::ErrorHandler { name, num, .. } => {
                    bind(Binding::ErrorHandler(name, num), element_ref, diags, span);
                }
                ElementKind::Parameters { .. } => {
                    bind(Binding::Parameters, element_ref, diags, span)
                }
                ElementKind::Invalid => {}
            }
        }

        // check if there is a start rule
        if !bindings.contains_key(&Binding::Term(symbols::START)) {
            diags.push(Diagnostic::missing_start_rule());
        }

        Self::update_ast(module, bindings, error_handler_map, diags);
    }

    fn update_ast(
        module: &mut Module<'a>,
        bindings: HashMap<Binding, ElementRef>,
        error_handler_map: Vec<RegexRef>,
        diags: &mut Vec<Diagnostic>,
    ) {
        for element in module.elements.iter_mut() {
            match element.kind {
                ElementKind::Start { ref mut action, .. } => {
                    if let Some(e) = bindings.get(&Binding::Action(symbols::START, 0)) {
                        *action = *e;
                    }
                }
                ElementKind::Rule {
                    name,
                    ref mut action,
                    ..
                } => {
                    if let Some(e) = bindings.get(&Binding::Action(name, 0)) {
                        *action = *e;
                    }
                }
                ElementKind::Action {
                    name,
                    ref name_span,
                    ..
                }
                | ElementKind::Predicate {
                    name,
                    ref name_span,
                    ..
                } => {
                    if !bindings.contains_key(&Binding::Term(name)) {
                        diags.push(Diagnostic::undefined_rule(name_span, name, &element.span));
                    }
                }
                _ => {}
            }
        }
        for (i, regex) in module.regexes.iter_mut().enumerate() {
            match regex.kind {
                RegexKind::Id {
                    name, ref mut elem, ..
                } => match bindings.get(&Binding::Term(name)) {
                    Some(e) => *elem = *e,
                    None => diags.push(Diagnostic::missing_definition(
                        &regex.span,
                        name.get_element_kind(),
                        name,
                    )),
                },
                RegexKind::Str { val, ref mut elem } => match bindings.get(&Binding::Token(val)) {
                    Some(e) => *elem = *e,
                    None => diags.push(Diagnostic::missing_definition(&regex.span, "token", val)),
                },
                RegexKind::Concat { ref mut error, .. } => {
                    *error = error_handler_map[i];
                }
                RegexKind::Or { ref mut error, .. } => {
                    *error = error_handler_map[i];
                }
                RegexKind::Action {
                    rule_name,
                    val,
                    ref mut elem,
                } => match bindings.get(&Binding::Action(rule_name, val)) {
                    Some(e) => *elem = *e,
                    None => diags.push(Diagnostic::missing_definition_warning(
                        &regex.span,
                        "action",
                        rule_name,
                        "#",
                        val,
                    )),
                },
                RegexKind::Predicate {
                    rule_name,
                    val,
                    ref mut elem,
                } => match bindings.get(&Binding::Predicate(rule_name, val)) {
                    Some(e) => *elem = *e,
                    None => diags.push(Diagnostic::missing_definition_warning(
                        &regex.span,
                        "predicate",
                        rule_name,
                        "?",
                        val,
                    )),
                },
                RegexKind::ErrorHandler {
                    rule_name,
                    val,
                    ref mut elem,
                    ..
                } => match bindings.get(&Binding::ErrorHandler(rule_name, val)) {
                    Some(e) => *elem = *e,
                    None => diags.push(Diagnostic::missing_definition_warning(
                        &regex.span,
                        "error handler",
                        rule_name,
                        "!",
                        val,
                    )),
                },
                _ => {}
            }
        }
    }

    fn check_element(module: &Module<'a>, element: ElementRef, diags: &mut Vec<Diagnostic>) {
        let obj = module.get_element(element).unwrap();
        match obj.kind {
            ElementKind::Start { regex, .. } => {
                let mut num = (1, 1, 1);
                Self::check_regex(module, regex, diags, &mut num, false, false);
            }
            ElementKind::Rule {
                name,
                ref name_span,
                regex,
                ..
            } => {
                if name.0.starts_with(|c: char| c.is_uppercase()) {
                    diags.push(Diagnostic::uppercase_rule(name_span, name));
                }
                let mut num = (1, 1, 1);
                Self::check_regex(module, regex, diags, &mut num, false, false);
            }
            ElementKind::Token { name, .. } => {
                // check if token name starts with uppercase letter
                if name.0.starts_with(|c: char| c.is_lowercase()) {
                    diags.push(Diagnostic::lowercase_token(&obj.span, name));
                }
            }
            _ => {}
        }
    }

    fn check_regex(
        module: &Module<'a>,
        regex: RegexRef,
        diags: &mut Vec<Diagnostic>,
        num: &mut (u64, u64, u64),
        in_alt: bool,
        in_loop: bool,
    ) {
        let obj = module.get_regex(regex).unwrap();
        match obj.kind {
            RegexKind::Concat { ref ops, .. } => {
                let mut in_alt = in_alt;
                let mut in_loop = in_loop;
                for op in ops {
                    Self::check_regex(module, *op, diags, num, in_alt, in_loop);
                    in_alt = false;
                    in_loop = false;
                }
            }
            RegexKind::Or { ref ops, .. } => {
                for op in ops {
                    Self::check_regex(module, *op, diags, num, true, false);
                }
            }
            RegexKind::Star { op } | RegexKind::Plus { op } | RegexKind::Option { op } => {
                Self::check_regex(module, op, diags, num, false, true);
            }
            RegexKind::Paren { op } => {
                Self::check_regex(module, op, diags, num, false, in_loop);
            }
            RegexKind::Action { val, .. } => {
                if num.0 == val && num.0 < u64::MAX {
                    num.0 += 1;
                } else if num.0 - 1 != val {
                    diags.push(Diagnostic::invalid_number(&obj.span, "action", "#", num.0));
                }
            }
            RegexKind::Predicate { val, .. } => {
                if !in_alt && !in_loop {
                    diags.push(Diagnostic::invalid_predicate_pos(&obj.span));
                }
                if num.1 == val && num.1 < u64::MAX {
                    num.1 += 1;
                } else if num.1 - 1 != val || val == 0 {
                    diags.push(Diagnostic::invalid_number(
                        &obj.span,
                        "predicate",
                        "?",
                        num.1,
                    ));
                }
            }
            RegexKind::ErrorHandler { val, .. } => {
                if num.2 == val && num.2 < u64::MAX {
                    num.2 += 1;
                } else if num.2 - 1 != val || val == 0 {
                    diags.push(Diagnostic::invalid_number(
                        &obj.span,
                        "error handler",
                        "!",
                        num.2,
                    ));
                }
            }
            _ => {}
        }
    }
}

struct LL1Validator;

impl<'a> LL1Validator {
    /// Validates that the grammar is an LL(1) grammar.
    fn run(module: &mut Module, diags: &mut Vec<Diagnostic>) {
        let mut first_sets = vec![BTreeSet::<Symbol>::new(); module.regexes.len()];
        let mut follow_sets = vec![BTreeSet::<Symbol>::new(); module.regexes.len()];
        Self::calc_first(module, &mut first_sets);
        Self::calc_follow(module, &first_sets, &mut follow_sets);
        for (i, regex) in module.regexes.iter_mut().enumerate() {
            std::mem::swap(&mut regex.first, &mut first_sets[i]);
            std::mem::swap(&mut regex.follow, &mut follow_sets[i]);
        }
        Self::check(module, diags);
    }

    /// Calculates the first set for each grammar rule.
    fn calc_first(module: &mut Module<'a>, first_sets: &mut [BTreeSet<Symbol<'a>>]) {
        // Iterates until there are no more changes in the first sets
        let mut change = true;
        while change {
            change = false;
            for element in module.elements.iter() {
                match element.kind {
                    ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                        Self::calc_first_regex(regex, first_sets, module, &mut change);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Calculates the first set for each regular expression within a rule.
    fn calc_first_regex(
        regex_ref: RegexRef,
        first_sets: &mut [BTreeSet<Symbol<'a>>],
        module: &Module<'a>,
        change: &mut bool,
    ) {
        let size = first_sets[regex_ref.0].len();
        match module.get_regex(regex_ref).unwrap().kind {
            RegexKind::Id { elem, .. } => match module.get_element(elem).unwrap().kind {
                ElementKind::Rule {
                    regex: rule_regex, ..
                } => {
                    let first = first_sets[rule_regex.0].clone();
                    first_sets[regex_ref.0].extend(first);
                }
                ElementKind::Token { name, .. } => {
                    first_sets[regex_ref.0].insert(name);
                }
                _ => unreachable!(),
            },
            RegexKind::Concat { ref ops, .. } => {
                let mut use_next = true;
                for op in ops {
                    Self::calc_first_regex(*op, first_sets, module, change);
                    // only add next first set if there was an epsilon
                    if use_next {
                        let op_first = first_sets[op.0].clone();
                        use_next = op_first.contains(&symbols::EMPTY);
                        first_sets[regex_ref.0].extend(op_first.into_iter());
                        first_sets[regex_ref.0].remove(&symbols::EMPTY);
                    }
                }
                if use_next {
                    first_sets[regex_ref.0].insert(symbols::EMPTY);
                }
            }
            RegexKind::Or { ref ops, .. } => {
                for op in ops {
                    if let RegexKind::ErrorHandler { .. } = module.get_regex(*op).unwrap().kind {
                        // error handler in alternation has empty first set to avoid conflicts
                    } else {
                        Self::calc_first_regex(*op, first_sets, module, change);
                        let op_first = first_sets[op.0].clone();
                        first_sets[regex_ref.0].extend(op_first.into_iter());
                    }
                }
            }
            RegexKind::Star { op } | RegexKind::Option { op } => {
                Self::calc_first_regex(op, first_sets, module, change);
                let op_first = first_sets[op.0].clone();
                let first = &mut first_sets[regex_ref.0];
                first.extend(op_first);
                first.insert(symbols::EMPTY);
            }
            RegexKind::Plus { op } | RegexKind::Paren { op } => {
                Self::calc_first_regex(op, first_sets, module, change);
                let op_first = first_sets[op.0].clone();
                first_sets[regex_ref.0].extend(op_first);
            }
            RegexKind::Str { elem, .. } => match module.get_element(elem).unwrap().kind {
                ElementKind::Token { name, .. } => {
                    first_sets[regex_ref.0].insert(name);
                }
                _ => unreachable!(),
            },
            _ => {
                first_sets[regex_ref.0].insert(symbols::EMPTY);
            }
        };
        *change |= first_sets[regex_ref.0].len() != size;
    }

    /// Calculates the follow set for each grammar rule.
    fn calc_follow(
        module: &Module<'a>,
        first_sets: &[BTreeSet<Symbol<'a>>],
        follow_sets: &mut [BTreeSet<Symbol<'a>>],
    ) {
        for element in module.elements.iter() {
            if let ElementKind::Start { regex, .. } = element.kind {
                follow_sets[regex.0].insert(symbols::EOF);
                break;
            }
        }
        // Iterates until there are no more changes in the follow sets
        let mut change = true;
        while change {
            change = false;
            for element in module.elements.iter() {
                match element.kind {
                    ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
                        Self::calc_follow_regex(
                            regex,
                            first_sets,
                            follow_sets,
                            module,
                            &mut change,
                        );
                    }
                    _ => {}
                }
            }
        }
    }

    /// Calculates the follow set for each regular expression within a rule.
    fn calc_follow_regex(
        regex_ref: RegexRef,
        first_sets: &[BTreeSet<Symbol<'a>>],
        follow_sets: &mut [BTreeSet<Symbol<'a>>],
        module: &Module<'a>,
        change: &mut bool,
    ) {
        match module.get_regex(regex_ref).unwrap().kind {
            RegexKind::Id { elem, .. } => {
                if let ElementKind::Rule {
                    regex: rule_regex, ..
                } = module.get_element(elem).unwrap().kind
                {
                    let size = follow_sets[rule_regex.0].len();
                    let follow = follow_sets[regex_ref.0].clone();
                    follow_sets[rule_regex.0].extend(follow);
                    *change |= size != follow_sets[rule_regex.0].len();
                }
            }
            RegexKind::Concat { ref ops, .. } => {
                let mut follow = follow_sets[regex_ref.0].clone();
                for op in ops.iter().rev() {
                    follow_sets[op.0].extend(follow.iter());
                    let op_first = &first_sets[op.0];
                    if op_first.contains(&symbols::EMPTY) {
                        follow.extend(op_first.iter());
                        follow.remove(&symbols::EMPTY);
                    } else {
                        follow.clone_from(op_first);
                    }
                    Self::calc_follow_regex(*op, first_sets, follow_sets, module, change);
                }
            }
            RegexKind::Or { ref ops, .. } => {
                let follow = follow_sets[regex_ref.0].clone();
                for op in ops {
                    follow_sets[op.0].extend(follow.iter());
                    Self::calc_follow_regex(*op, first_sets, follow_sets, module, change);
                }
            }
            RegexKind::Star { op } => {
                let follow = follow_sets[regex_ref.0].clone();
                let op_first = &first_sets[op.0];
                let op_follow = &mut follow_sets[op.0];
                op_follow.extend(op_first.iter());
                op_follow.remove(&symbols::EMPTY);
                op_follow.extend(follow);
                Self::calc_follow_regex(op, first_sets, follow_sets, module, change);
            }
            RegexKind::Plus { op } => {
                let first = &first_sets[regex_ref.0];
                let follow = follow_sets[regex_ref.0].clone();
                let op_follow = &mut follow_sets[op.0];
                op_follow.extend(first.iter());
                op_follow.extend(follow);
                Self::calc_follow_regex(op, first_sets, follow_sets, module, change);
            }
            RegexKind::Option { op } | RegexKind::Paren { op } => {
                let follow = follow_sets[regex_ref.0].clone();
                follow_sets[op.0].extend(follow);
                Self::calc_follow_regex(op, first_sets, follow_sets, module, change);
            }
            _ => {}
        };
    }

    /// Checks if LL(1) condition holds for the all regexes.
    fn check(module: &Module, diags: &mut Vec<Diagnostic>) {
        for index in 0..module.regexes.len() {
            let regex_ref = RegexRef::from(index);
            Self::check_regex(regex_ref, module, diags);
        }
    }

    fn has_predicate(regex_ref: RegexRef, module: &Module<'a>) -> bool {
        match module.get_regex(regex_ref).unwrap().kind {
            RegexKind::Concat { ref ops, .. } => matches!(
                module.get_regex(ops[0]).unwrap().kind,
                RegexKind::Predicate { .. }
            ),
            RegexKind::Paren { op } => Self::has_predicate(op, module),
            _ => false,
        }
    }

    /// Checks if LL(1) condition holds for the regex.
    fn check_regex(regex_ref: RegexRef, module: &Module<'a>, diags: &mut Vec<Diagnostic>) {
        let regex = module.get_regex(regex_ref).unwrap();
        match regex.kind {
            RegexKind::Or { ref ops, .. } => {
                for i in 0..ops.len() {
                    let op = module.get_regex(ops[i]).unwrap();
                    let prediction = op.predict();
                    if Self::has_predicate(ops[i], module) {
                        continue;
                    }
                    let mut related = vec![];
                    for op in ops.iter().skip(i + 1) {
                        let intersection = prediction
                            .intersection(&module.get_regex(*op).unwrap().predict())
                            .copied()
                            .collect::<BTreeSet<_>>();
                        if !intersection.is_empty() {
                            let set = format!("with token set: {:?}", intersection);
                            related.push((module.get_regex(*op).unwrap().span.clone(), set));
                        }
                    }
                    if !related.is_empty() {
                        diags.push(Diagnostic::ll1_conflict_alt(&op.span, related));
                    }
                }
            }
            RegexKind::Star { op } | RegexKind::Plus { op } | RegexKind::Option { op } => {
                let intersection = module
                    .get_regex(regex_ref)
                    .unwrap()
                    .follow
                    .intersection(&module.get_regex(op).unwrap().predict())
                    .copied()
                    .collect::<BTreeSet<_>>();
                if !Self::has_predicate(op, module) && !intersection.is_empty() {
                    let set = format!("with token set: {:?}", intersection);
                    diags.push(if matches!(regex.kind, RegexKind::Option { .. }) {
                        Diagnostic::ll1_conflict_opt(&regex.span, set)
                    } else {
                        Diagnostic::ll1_conflict_rep(&regex.span, set)
                    });
                }
            }
            RegexKind::Id { .. } => {
                if module.get_regex(regex_ref).unwrap().first.is_empty() {
                    diags.push(Diagnostic::consume_tokens(&regex.span));
                }
            }
            _ => {}
        };
    }
}

struct UsageValidator;

impl UsageValidator {
    fn run(module: &mut Module, diags: &mut Vec<Diagnostic>) {
        let mut used = vec![false; module.elements.len()];
        let mut change = true;
        while change {
            change = false;
            for (index, element) in module.elements.iter().enumerate() {
                match &element.kind {
                    ElementKind::Start { regex, .. } => {
                        used[index] = true;
                        Self::check_regex(module, *regex, &mut change, &mut used);
                    }
                    ElementKind::Rule { regex, name, .. } => {
                        if name.0.starts_with('_') {
                            used[index] = true;
                        }
                        if used[index] {
                            Self::check_regex(module, *regex, &mut change, &mut used);
                        }
                    }
                    ElementKind::Token { name, .. } => {
                        if name.0.starts_with('_') {
                            used[index] = true;
                        }
                    }
                    ElementKind::Action { num, .. }
                    | ElementKind::Predicate { num, .. }
                    | ElementKind::ErrorHandler { num, .. }
                        if *num != 0 => {}
                    _ => used[index] = true,
                };
            }
        }
        for (index, element) in module.elements.iter_mut().enumerate() {
            if used[index] {
                element.used = true;
            } else {
                diags.push(Diagnostic::unused_element(&element.span));
            }
        }
    }

    fn check_regex(module: &Module, regex: RegexRef, change: &mut bool, used: &mut [bool]) {
        match module.get_regex(regex).unwrap().kind {
            RegexKind::Id { elem, .. } | RegexKind::Str { elem, .. } => {
                if elem.is_valid() {
                    *change |= !used[elem.0];
                    used[elem.0] = true;
                }
            }
            RegexKind::Concat { ref ops, .. } | RegexKind::Or { ref ops, .. } => {
                for op in ops {
                    Self::check_regex(module, *op, change, used);
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                Self::check_regex(module, op, change, used);
            }
            RegexKind::Action { elem, .. }
            | RegexKind::Predicate { elem, .. }
            | RegexKind::ErrorHandler { elem, .. } => {
                if elem.is_valid() {
                    *change |= !used[elem.0];
                    used[elem.0] = true;
                }
            }
            _ => {}
        }
    }
}

impl std::hash::Hash for RegexRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[derive(Default)]
struct CancelSetGenerator {
    dom: HashMap<RegexRef, HashSet<RegexRef>>,
    pred: HashMap<RegexRef, HashSet<RegexRef>>,
}

#[allow(clippy::mutable_key_type)]
impl CancelSetGenerator {
    fn new() -> Self {
        Self::default()
    }
    fn run(&mut self, module: &mut Module) {
        let mut start = None;
        for element in module.elements.iter() {
            match element.kind {
                ElementKind::Start { regex, .. } => {
                    start = Some(regex);
                    self.set_regex_pred(module, regex);
                }
                ElementKind::Rule { regex, .. } => {
                    if element.used {
                        self.set_regex_pred(module, regex);
                    }
                }
                _ => {}
            };
        }
        // there must be a start rule at this point of the semantic pass
        let start = start.unwrap();

        let nodes_no_start: HashSet<_> = self.pred.keys().copied().collect();
        let mut nodes = nodes_no_start.clone();
        nodes.insert(start);

        // start node dominates itself
        self.dom.insert(start, HashSet::from_iter([start]));
        // other nodes are initialized with all nodes as dominators
        for regex in nodes_no_start.iter() {
            self.dom.insert(*regex, nodes.clone());
        }
        // iteratively eliminate nodes that are not dominators
        let mut change = true;
        while change {
            change = false;
            for regex in nodes_no_start.iter() {
                let mut pred = self.pred[regex].iter();
                if let Some(p) = pred.next() {
                    let mut dom = self.dom[p].clone();
                    for p in pred {
                        dom = dom.intersection(&self.dom[p]).copied().collect();
                    }
                    dom.insert(*regex);
                    change |= dom.len() != self.dom[regex].len();
                    if change {
                        self.dom.insert(*regex, dom);
                    }
                } else {
                    self.dom.insert(*regex, HashSet::from_iter([*regex]));
                }
            }
        }
        // extract the error handler of the regex if there is one
        let get_error_handler = |regex: RegexRef| -> Option<RegexRef> {
            match module.get_regex(regex).unwrap().kind {
                RegexKind::Concat { error, .. } | RegexKind::Or { error, .. }
                    if error.is_valid() =>
                {
                    Some(error)
                }
                _ => None,
            }
        };
        // calculate cancel set for error handlers
        let mut cancel_sets = vec![BTreeSet::<Symbol>::new(); module.regexes.len()];
        for regex in nodes_no_start.iter() {
            if let Some(error) = get_error_handler(*regex) {
                for dom in self.dom[regex].iter() {
                    if get_error_handler(*dom).is_none() && *dom != start {
                        continue;
                    }
                    cancel_sets[error.0].extend(module.get_regex(*dom).unwrap().follow.iter());
                }
                for sym in module.get_regex(*regex).unwrap().follow.iter() {
                    cancel_sets[error.0].remove(sym);
                }
            }
        }
        for (index, regex) in module.regexes.iter_mut().enumerate() {
            std::mem::swap(&mut regex.cancel, &mut cancel_sets[index]);
        }
    }

    fn add_pred(&mut self, r: RegexRef, p: RegexRef) {
        if let Some(pred) = self.pred.get_mut(&r) {
            pred.insert(p);
        } else {
            self.pred.insert(r, HashSet::from_iter([p]));
        }
    }

    fn set_regex_pred(&mut self, module: &Module, regex: RegexRef) {
        match module.get_regex(regex).unwrap().kind {
            RegexKind::Id { elem, .. } => {
                if let Some(Element {
                    kind: ElementKind::Rule { regex: op, .. },
                    ..
                }) = module.get_element(elem)
                {
                    self.add_pred(*op, regex);
                }
            }
            RegexKind::Concat { ref ops, .. } | RegexKind::Or { ref ops, .. } => {
                for op in ops {
                    self.add_pred(*op, regex);
                    self.set_regex_pred(module, *op);
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                self.add_pred(op, regex);
                self.set_regex_pred(module, op);
            }
            _ => {}
        }
    }
}
