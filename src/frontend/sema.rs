use super::ast::*;
use super::diag::*;
use super::symbol::*;
use super::token::*;
use std::collections::{BTreeSet, HashMap};

pub struct SemanticPass {}

impl SemanticPass {
    pub fn run(module: &Module, diag: &mut Diag) {
        GeneralValidator::run(module, diag);
        if diag.has_errors() {
            return;
        }
        LL1Validator::run(module, diag);
        UsageValidator::run(module, diag);
    }
}

/// general validation of the AST
struct GeneralValidator {}

#[derive(Hash, PartialEq, Eq, Copy, Clone, Debug)]
pub enum Binding {
    Term(Symbol),
    Token(Symbol),
    Action(Symbol, u64),
    Predicate(Symbol, u64),
    ErrorHandler(Symbol, u64),
}

impl std::fmt::Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Term(name) => write!(f, "element '{}'", name),
            Self::Token(name) => write!(f, "token string '{}'", name),
            Self::Action(name, num) => write!(f, "semantic action '{}#{}'", name, num),
            Self::Predicate(name, num) => write!(f, "semantic predicate '{}?{}'", name, num),
            Self::ErrorHandler(name, num) => write!(f, "error handler '{}!{}'", name, num),
        }
    }
}

impl<'a, 'b> GeneralValidator {
    fn run(module: &'a Module<'b>, diag: &mut Diag) {
        // bind rule names to corresponding elements
        let bindings = Self::bind_elements(module, diag);

        // check if there is a start rule
        if !bindings.contains_key(&Binding::Term(Symbol::START)) {
            diag.error(Code::MissingStart, Range::default());
        }

        if let Some(preamble) = bindings.get(&Binding::Term(Symbol::PREAMBLE)) {
            module.preamble.set(Some(*preamble));
        }
        if let Some(parameters) = bindings.get(&Binding::Term(Symbol::PARAMETERS)) {
            module.parameters.set(Some(*parameters));
        }
        if let Some(error) = bindings.get(&Binding::Term(Symbol::ERROR)) {
            module.error.set(Some(*error));
        }
        if let Some(limit) = bindings.get(&Binding::Term(Symbol::LIMIT)) {
            module.limit.set(Some(*limit));
        }
        if let Some(language) = bindings.get(&Binding::Term(Symbol::LANGUGAE)) {
            module.language.set(Some(*language));
            if let ElementKind::Language { name } = language.kind {
                match name.as_str() {
                    "rust" => {}
                    _ => {
                        diag.error(Code::InvalidLang(name), language.range());
                    }
                }
            }
        }

        // check elements
        for element in module.elements.iter() {
            Self::check_element(element, diag, &bindings);
        }
    }

    fn bind(
        binding: Binding,
        element: &'b Element<'b>,
        bindings: &mut HashMap<Binding, &'b Element<'b>>,
        diag: &mut Diag,
    ) {
        if let Some(old) = bindings.insert(binding, element) {
            diag.error_with_related(
                Code::Redefinition(binding),
                element.range(),
                vec![(old.range(), "from here".to_string())],
            );
        }
    }

    fn bind_elements(module: &'a Module<'b>, diag: &mut Diag) -> HashMap<Binding, &'b Element<'b>> {
        let mut bindings = HashMap::new();
        for element in module.elements.iter() {
            match element.kind {
                ElementKind::Start { .. } => {
                    Self::bind(Binding::Term(Symbol::START), element, &mut bindings, diag);
                }
                ElementKind::Rule { name, .. } => {
                    Self::bind(Binding::Term(name), element, &mut bindings, diag);
                }
                ElementKind::Token { name, sym, .. } => {
                    if name == Symbol::EOF || name == Symbol::INVALID {
                        diag.error(Code::PredefToken, element.range());
                        continue;
                    }
                    Self::bind(Binding::Term(name), element, &mut bindings, diag);
                    if sym != Symbol::EMPTY {
                        Self::bind(Binding::Token(sym), element, &mut bindings, diag);
                    }
                }
                ElementKind::Action { name, num, .. } => {
                    Self::bind(Binding::Action(name, num), element, &mut bindings, diag);
                }
                ElementKind::Predicate { name, num, .. } => {
                    Self::bind(Binding::Predicate(name, num), element, &mut bindings, diag);
                }
                ElementKind::ErrorHandler { name, num, .. } => {
                    Self::bind(
                        Binding::ErrorHandler(name, num),
                        element,
                        &mut bindings,
                        diag,
                    );
                }
                ElementKind::Preamble { .. } => {
                    Self::bind(
                        Binding::Term(Symbol::PREAMBLE),
                        element,
                        &mut bindings,
                        diag,
                    );
                }
                ElementKind::Parameters { .. } => {
                    Self::bind(
                        Binding::Term(Symbol::PARAMETERS),
                        element,
                        &mut bindings,
                        diag,
                    );
                }
                ElementKind::ErrorCode { .. } => {
                    Self::bind(Binding::Term(Symbol::ERROR), element, &mut bindings, diag);
                }
                ElementKind::Limit { .. } => {
                    Self::bind(Binding::Term(Symbol::LIMIT), element, &mut bindings, diag);
                }
                ElementKind::Language { .. } => {
                    Self::bind(
                        Binding::Term(Symbol::LANGUGAE),
                        element,
                        &mut bindings,
                        diag,
                    );
                }
                _ => {}
            }
        }
        bindings
    }

    fn check_element(
        element: &'b Element<'b>,
        diag: &mut Diag,
        bindings: &HashMap<Binding, &'b Element<'b>>,
    ) {
        match &element.kind {
            ElementKind::Start { regex, action, .. } => {
                if let Some(e) = bindings.get(&Binding::Action(Symbol::START, 0)) {
                    action.set(e)
                }
                let mut num = (1, 1, 1);
                Self::check_regex(regex, diag, Symbol::START, &mut num, None, false, bindings);
            }
            ElementKind::Rule {
                name,
                regex,
                action,
                ..
            } => {
                if name
                    .as_str()
                    .trim_start_matches('_')
                    .starts_with(|c: char| c.is_uppercase())
                {
                    diag.error(Code::UppercaseRule(*name), element.range());
                }
                if let Some(e) = bindings.get(&Binding::Action(*name, 0)) {
                    action.set(e)
                }
                let mut num = (1, 1, 1);
                Self::check_regex(regex, diag, *name, &mut num, None, false, bindings);
            }
            ElementKind::Token { name, .. } => {
                // check if token name starts with uppercase letter
                if name
                    .as_str()
                    .trim_start_matches('_')
                    .starts_with(|c: char| c.is_lowercase())
                {
                    diag.error(Code::LowercaseToken(*name), element.range());
                }
            }
            ElementKind::Action { name, .. } | ElementKind::Predicate { name, .. } => {
                if !bindings.contains_key(&Binding::Term(*name)) {
                    diag.error(Code::UndefinedElement(*name), element.range());
                }
            }
            _ => {}
        }
    }

    fn check_regex(
        regex: &'b Regex<'b>,
        diag: &mut Diag,
        name: Symbol,
        num: &mut (u64, u64, u64),
        alt: Option<&'b Regex<'b>>,
        in_loop: bool,
        bindings: &HashMap<Binding, &'b Element<'b>>,
    ) {
        match &regex.kind {
            RegexKind::Id { name, elem, .. } => match bindings.get(&Binding::Term(*name)) {
                Some(e) => elem.set(e),
                None => diag.error(Code::UndefinedElement(*name), regex.range()),
            },
            RegexKind::Str { val, elem } => match bindings.get(&Binding::Token(*val)) {
                Some(e) => elem.set(e),
                None => diag.error(Code::UndefinedElement(*val), regex.range()),
            },
            RegexKind::Concat { ops } => {
                let mut alt = alt;
                let mut in_loop = in_loop;
                for op in ops {
                    if let RegexKind::ErrorHandler { .. } = op.kind {
                        diag.error(Code::ErrorSyntax, regex.range());
                    }
                    Self::check_regex(op, diag, name, num, alt, in_loop, bindings);
                    alt = None;
                    in_loop = false;
                }
            }
            RegexKind::Or { ops, .. } => {
                for op in ops {
                    Self::check_regex(op, diag, name, num, Some(regex), false, bindings);
                }
            }
            RegexKind::Star { op } | RegexKind::Plus { op } | RegexKind::Option { op } => {
                Self::check_regex(op, diag, name, num, None, true, bindings);
            }
            RegexKind::Paren { op } => {
                Self::check_regex(op, diag, name, num, None, in_loop, bindings);
            }
            RegexKind::Action { val, elem } => {
                match bindings.get(&Binding::Action(name, *val)) {
                    Some(e) => elem.set(e),
                    None => diag.warning(Code::UndefinedAction, regex.range()),
                }
                if num.0 == *val {
                    num.0 += 1;
                } else {
                    diag.error(Code::ExpectedAction(num.0), regex.range());
                }
            }
            RegexKind::Predicate { val, elem } => {
                if alt.is_none() && !in_loop {
                    diag.error(Code::PredPosition, regex.range());
                }
                match bindings.get(&Binding::Predicate(name, *val)) {
                    Some(e) => elem.set(e),
                    None => diag.warning(Code::UndefinedPredicate, regex.range()),
                }
                if num.1 == *val {
                    num.1 += 1;
                } else {
                    diag.error(Code::ExpectedPredicate(num.1), regex.range());
                }
            }
            RegexKind::ErrorHandler { val, elem } => {
                match bindings.get(&Binding::ErrorHandler(name, *val)) {
                    Some(e) => elem.set(e),
                    None => diag.warning(Code::UndefinedErrorHandler, regex.range()),
                }
                if let Some(Regex {
                    kind: RegexKind::Or { error, .. },
                    ..
                }) = alt
                {
                    if error.get().is_some() {
                        diag.error(Code::ErrorCount, regex.range());
                    }
                    error.set(regex);
                }
                if num.2 == *val {
                    num.2 += 1;
                } else {
                    diag.error(Code::ExpectedErrorHandler(num.2), regex.range());
                }
            }
            _ => {}
        }
    }
}

struct LL1Validator {}

impl<'a> LL1Validator {
    /// Validates that the grammar is an LL(1) grammar.
    fn run(module: &Module, diag: &mut Diag) {
        Self::calc_first_elements(module);
        Self::calc_follow_elements(module);
        Self::check_elements(module, diag);
    }

    /// Calculates the first set for each grammar rule.
    fn calc_first_elements(module: &Module) {
        // Iterates until there are no more changes in the first sets
        let mut change = true;
        while change {
            change = false;
            for element in module.elements.iter() {
                match element.kind {
                    ElementKind::Start { regex, .. } => {
                        Self::calc_first_regex(regex, &mut change);
                    }
                    ElementKind::Rule { regex, .. } => {
                        Self::calc_first_regex(regex, &mut change);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Calculates the first set for each regular expression within a rule.
    fn calc_first_regex(regex: &'a Regex<'a>, change: &mut bool) {
        let size = regex.first().len();
        match &regex.kind {
            RegexKind::Id { elem, .. } => match elem.get().unwrap().kind {
                ElementKind::Rule {
                    regex: rule_regex, ..
                } => {
                    let first = rule_regex.first().clone();
                    regex.first_mut().extend(first.into_iter());
                }
                ElementKind::Token { name, .. } => {
                    regex.first_mut().insert(name);
                }
                _ => unreachable!(),
            },
            RegexKind::Concat { ops } => {
                let mut use_next = true;
                for op in ops {
                    Self::calc_first_regex(op, change);
                    // only add next first set if there was an epsilon
                    if use_next {
                        use_next = op.first().contains(&Symbol::EMPTY);
                        regex.first_mut().extend(op.first().iter());
                        regex.first_mut().remove(&Symbol::EMPTY);
                    }
                }
                if use_next {
                    regex.first_mut().insert(Symbol::EMPTY);
                }
            }
            RegexKind::Or { ops, .. } => {
                for op in ops {
                    Self::calc_first_regex(op, change);
                    regex.first_mut().extend(op.first().iter());
                }
            }
            RegexKind::Star { op } | RegexKind::Option { op } => {
                Self::calc_first_regex(op, change);
                regex.first_mut().extend(op.first().iter());
                regex.first_mut().insert(Symbol::EMPTY);
            }
            RegexKind::Plus { op } | RegexKind::Paren { op } => {
                Self::calc_first_regex(op, change);
                regex.first_mut().extend(op.first().iter());
            }
            RegexKind::Str { elem, .. } => match elem.get().unwrap().kind {
                ElementKind::Token { name, .. } => {
                    regex.first_mut().insert(name);
                }
                _ => unreachable!(),
            },
            RegexKind::ErrorHandler { .. } => {}
            _ => {
                regex.first_mut().insert(Symbol::EMPTY);
            }
        };
        *change |= regex.first().len() != size;
    }

    /// Calculates the follow set for each grammar rule.
    fn calc_follow_elements(module: &Module) {
        // Iterates until there are no more changes in the follow sets
        let mut change = true;
        while change {
            change = false;
            for element in module.elements.iter() {
                match element.kind {
                    ElementKind::Start { regex, .. } => {
                        regex.follow_mut().insert(Symbol::EOF);
                        Self::calc_follow_regex(regex, &mut change);
                    }
                    ElementKind::Rule { regex, .. } => {
                        Self::calc_follow_regex(regex, &mut change);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Calculates the follow set for each regular expression within a rule.
    fn calc_follow_regex(regex: &'a Regex<'a>, change: &mut bool) {
        match &regex.kind {
            RegexKind::Id { elem, .. } => {
                if let ElementKind::Rule {
                    regex: rule_regex, ..
                } = elem.get().unwrap().kind
                {
                    let size = rule_regex.follow().len();
                    let follow = regex.follow().clone();
                    rule_regex.follow_mut().extend(follow.into_iter());
                    *change |= size != rule_regex.follow().len();
                }
            }
            RegexKind::Concat { ops } => {
                let mut follow = regex.follow().clone();
                for op in ops.iter().rev() {
                    op.follow_mut().extend(follow.iter());
                    if op.first().contains(&Symbol::EMPTY) {
                        follow.extend(op.first().iter());
                        follow.remove(&Symbol::EMPTY);
                    } else {
                        follow.clone_from(&op.first());
                    }
                    Self::calc_follow_regex(op, change);
                }
            }
            RegexKind::Or { ops, .. } => {
                for op in ops {
                    op.follow_mut().extend(regex.follow().iter());
                    Self::calc_follow_regex(op, change);
                }
            }
            RegexKind::Star { op } => {
                op.follow_mut().extend(regex.first().iter());
                op.follow_mut().remove(&Symbol::EMPTY);
                op.follow_mut().extend(regex.follow().iter());
                Self::calc_follow_regex(op, change);
            }
            RegexKind::Plus { op } => {
                op.follow_mut().extend(regex.first().iter());
                op.follow_mut().extend(regex.follow().iter());
                Self::calc_follow_regex(op, change);
            }
            RegexKind::Option { op } | RegexKind::Paren { op } => {
                op.follow_mut().extend(regex.follow().iter());
                Self::calc_follow_regex(op, change);
            }
            _ => {}
        };
    }

    /// Checks if LL(1) condition holds for the element regex.
    fn check_elements(module: &Module, diag: &mut Diag) {
        for element in module.elements.iter() {
            match element.kind {
                ElementKind::Start { regex, .. } => {
                    Self::check_regex(regex, diag);
                }
                ElementKind::Rule { regex, .. } => {
                    Self::check_regex(regex, diag);
                }
                _ => {}
            }
        }
    }

    fn has_predicate(regex: &'a Regex<'a>) -> bool {
        match &regex.kind {
            RegexKind::Concat { ops } => matches!(&ops[0].kind, RegexKind::Predicate { .. }),
            RegexKind::Paren { op } => Self::has_predicate(op),
            _ => false,
        }
    }

    /// Checks if LL(1) condition holds for the regex.
    fn check_regex(regex: &'a Regex<'a>, diag: &mut Diag) {
        match &regex.kind {
            RegexKind::Concat { ops } => {
                for op in ops {
                    Self::check_regex(op, diag);
                }
            }
            RegexKind::Or { ops, .. } => {
                for i in 0..ops.len() {
                    let prediction = ops[i].predict();
                    if Self::has_predicate(ops[i]) {
                        continue;
                    }
                    let mut related = vec![];
                    for j in i + 1..ops.len() {
                        let intersection = prediction
                            .intersection(&ops[j].predict())
                            .copied()
                            .collect::<BTreeSet<_>>();
                        if !intersection.is_empty() {
                            let set = format!("with token set: {:?}", intersection);
                            related.push((ops[j].range(), set));
                        }
                    }
                    if !related.is_empty() {
                        diag.error_with_related(Code::LL1Conflict, ops[i].range(), related);
                    }
                    Self::check_regex(ops[i], diag);
                }
            }
            RegexKind::Star { op } | RegexKind::Plus { op } | RegexKind::Option { op } => {
                let intersection = regex
                    .follow()
                    .intersection(&op.predict())
                    .copied()
                    .collect::<BTreeSet<_>>();
                if !Self::has_predicate(op) && !intersection.is_empty() {
                    let set = format!("with token set: {:?}", intersection);
                    diag.error_with_related(
                        Code::LL1Conflict,
                        op.range(),
                        vec![(regex.range(), set)],
                    );
                }
                Self::check_regex(op, diag);
            }
            RegexKind::Paren { op } => {
                Self::check_regex(op, diag);
            }
            RegexKind::Id { .. } => {
                if regex.first().is_empty() {
                    diag.error(Code::ConsumeTokens, regex.range());
                }
            }
            _ => {}
        };
    }
}

struct UsageValidator {}

impl UsageValidator {
    fn run(module: &Module, diag: &mut Diag) {
        for element in module.elements.iter() {
            match &element.kind {
                ElementKind::Start { regex, .. } => {
                    element.attr.used.set(true);
                    Self::check_regex(regex);
                }
                ElementKind::Rule { regex, name, .. } => {
                    let c = name.to_string().chars().next().unwrap();
                    if c == '_' {
                        element.attr.used.set(true)
                    }
                    if !regex.follow().is_empty() {
                        Self::check_regex(regex);
                    }
                }
                ElementKind::Token { name, .. } => {
                    let c = name.to_string().chars().next().unwrap();
                    if c == '_' {
                        element.attr.used.set(true)
                    }
                }
                ElementKind::Action { num, .. } | ElementKind::Predicate { num, .. }
                    if *num != 0 => {}
                _ => element.attr.used.set(true),
            };
        }
        for element in module.elements.iter() {
            if !element.attr.used.get() {
                diag.warning(Code::UnusedElement, element.range());
            }
        }
    }

    /// Calculates the first set for each regular expression within a rule.
    fn check_regex(regex: &Regex) {
        match &regex.kind {
            RegexKind::Id { elem, .. } | RegexKind::Str { elem, .. } => {
                elem.get().unwrap().attr.used.set(true);
            }
            RegexKind::Concat { ops } | RegexKind::Or { ops, .. } => {
                for op in ops {
                    Self::check_regex(op);
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                Self::check_regex(op);
            }
            RegexKind::Action { elem, .. } | RegexKind::Predicate { elem, .. } => {
                if let Some(elem) = elem.get() {
                    elem.attr.used.set(true);
                }
            }
            _ => {}
        }
    }
}
