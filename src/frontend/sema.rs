use super::ast::*;
use super::diag::*;
use super::symbol::*;
use super::token::*;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::iter::FromIterator;

pub struct SemanticPass {}

impl<'a> SemanticPass {
    pub fn run(module: &'a Module<'a>, diag: &mut Diag) {
        GeneralValidator::run(module, diag);
        if diag.has_errors() {
            return;
        }
        UsageValidator::run(module, diag);
        LL1Validator::run(module, diag);
        CancelSetGenerator::new().run(module);
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
            RegexKind::Concat { ops, error } => {
                let mut alt = alt;
                let mut in_loop = in_loop;
                for (i, op) in ops.iter().enumerate() {
                    if let RegexKind::ErrorHandler { .. } = op.kind {
                        if i != ops.len() - 1 {
                            // only allow error handler as last term in concatenation
                            diag.error(Code::ErrorSyntax, regex.range());
                        } else {
                            error.set(op);
                        }
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
                if num.0 == *val && num.0 < u64::MAX {
                    num.0 += 1;
                } else if num.0 - 1 != *val {
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
                if num.1 == *val && num.1 < u64::MAX {
                    num.1 += 1;
                } else if num.1 - 1 != *val || *val == 0 {
                    diag.error(Code::ExpectedPredicate(num.1), regex.range());
                }
            }
            RegexKind::ErrorHandler { val, elem, .. } => {
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
                if num.2 == *val && num.2 < u64::MAX {
                    num.2 += 1;
                } else if num.2 - 1 != *val || *val == 0 {
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
                    ElementKind::Start { regex, .. } | ElementKind::Rule { regex, .. } => {
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
            RegexKind::Concat { ops, .. } => {
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
                    if let RegexKind::ErrorHandler { .. } = op.kind {
                        // error handler in alternation has empty first set to avoid conflicts
                    } else {
                        Self::calc_first_regex(op, change);
                        regex.first_mut().extend(op.first().iter());
                    }
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
            RegexKind::Concat { ops, .. } => {
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
            RegexKind::Concat { ops, .. } => matches!(&ops[0].kind, RegexKind::Predicate { .. }),
            RegexKind::Paren { op } => Self::has_predicate(op),
            _ => false,
        }
    }

    /// Checks if LL(1) condition holds for the regex.
    fn check_regex(regex: &'a Regex<'a>, diag: &mut Diag) {
        match &regex.kind {
            RegexKind::Concat { ops, .. } => {
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
        let mut change = true;
        while change {
            change = false;
            for element in module.elements.iter() {
                match &element.kind {
                    ElementKind::Start { regex, .. } => {
                        element.attr.used.set(true);
                        Self::check_regex(regex, &mut change);
                    }
                    ElementKind::Rule { regex, name, .. } => {
                        if name.as_str().starts_with('_') {
                            element.attr.used.set(true)
                        }
                        if element.attr.used.get() {
                            Self::check_regex(regex, &mut change);
                        }
                    }
                    ElementKind::Token { name, .. } => {
                        if name.as_str().starts_with('_') {
                            element.attr.used.set(true)
                        }
                    }
                    ElementKind::Action { num, .. }
                    | ElementKind::Predicate { num, .. }
                    | ElementKind::ErrorHandler { num, .. }
                        if *num != 0 => {}
                    _ => element.attr.used.set(true),
                };
            }
        }
        for element in module.elements.iter() {
            if !element.attr.used.get() {
                diag.warning(Code::UnusedElement, element.range());
            }
        }
    }

    fn check_regex(regex: &Regex, change: &mut bool) {
        match &regex.kind {
            RegexKind::Id { elem, .. } | RegexKind::Str { elem, .. } => {
                if let Some(elem) = elem.get() {
                    *change |= !elem.attr.used.get();
                    elem.attr.used.set(true);
                }
            }
            RegexKind::Concat { ops, .. } | RegexKind::Or { ops, .. } => {
                for op in ops {
                    Self::check_regex(op, change);
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                Self::check_regex(op, change);
            }
            RegexKind::Action { elem, .. }
            | RegexKind::Predicate { elem, .. }
            | RegexKind::ErrorHandler { elem, .. } => {
                if let Some(elem) = elem.get() {
                    *change |= !elem.attr.used.get();
                    elem.attr.used.set(true);
                }
            }
            _ => {}
        }
    }
}

impl<'a> std::hash::Hash for Regex<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self as *const Self).hash(state)
    }
}

impl<'a> PartialEq<Regex<'a>> for Regex<'a> {
    fn eq(&self, other: &Regex<'a>) -> bool {
        self as *const Self == other as *const Self
    }
}

impl<'a> Eq for Regex<'a> {}

#[derive(Default)]
struct CancelSetGenerator<'a> {
    dom: HashMap<&'a Regex<'a>, HashSet<&'a Regex<'a>>>,
    pred: HashMap<&'a Regex<'a>, HashSet<&'a Regex<'a>>>,
}

impl<'a> CancelSetGenerator<'a> {
    fn new() -> Self {
        Self::default()
    }
    fn run(&mut self, module: &'a Module<'a>) {
        let mut start = None;
        for element in module.elements.iter() {
            match &element.kind {
                ElementKind::Start { regex, .. } => {
                    start = Some(*regex);
                    self.set_regex_pred(regex);
                }
                ElementKind::Rule { regex, .. } => {
                    if element.attr.used.get() {
                        self.set_regex_pred(regex);
                    }
                }
                _ => {}
            };
        }
        // there must be a start rule at this point of the semantic pass
        let start = start.unwrap();

        let nodes_no_start: HashSet<_> = self.pred.keys().map(|k| *k).collect();
        let mut nodes = nodes_no_start.clone();
        nodes.insert(start);

        // start node dominates itself
        self.dom
            .insert(start, HashSet::from_iter([start]));
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
                        dom = dom.intersection(&self.dom[p]).map(|r| *r).collect();
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
        let get_error_handler = |regex: &'a Regex<'a>| -> Option<&'a Regex<'a>> {
            match &regex.kind {
                RegexKind::Concat { error, .. } | RegexKind::Or { error, .. }
                    if error.get().is_some() =>
                {
                    error.get()
                }
                _ => None,
            }
        };
        // calculate cancel set for error handlers
        for regex in nodes_no_start.iter() {
            if let Some(error) = get_error_handler(regex) {
                let mut cancel = error.cancel_mut();
                for dom in self.dom[regex].iter() {
                    if get_error_handler(dom).is_none() && *dom != start {
                        continue;
                    }
                    cancel.extend(dom.follow().iter());
                }
                for sym in regex.follow().iter() {
                    cancel.remove(sym);
                }
            }
        }
    }

    fn add_pred(&mut self, r: &'a Regex<'a>, p: &'a Regex<'a>) {
        if let Some(pred) = self.pred.get_mut(&r) {
            pred.insert(p);
        } else {
            self.pred.insert(r, HashSet::from_iter([p]));
        }
    }

    fn set_regex_pred(&mut self, regex: &'a Regex<'a>) {
        match &regex.kind {
            RegexKind::Id { elem, .. } => {
                if let Some(Element {
                    kind: ElementKind::Rule { regex: op, .. },
                    ..
                }) = elem.get()
                {
                    self.add_pred(op, regex);
                }
            }
            RegexKind::Concat { ops, .. } | RegexKind::Or { ops, .. } => {
                for op in ops {
                    self.add_pred(op, regex);
                    self.set_regex_pred(op);
                }
            }
            RegexKind::Star { op }
            | RegexKind::Plus { op }
            | RegexKind::Option { op }
            | RegexKind::Paren { op } => {
                self.add_pred(op, regex);
                self.set_regex_pred(op);
            }
            _ => {}
        }
    }
}
