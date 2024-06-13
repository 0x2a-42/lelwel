use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use codespan_reporting::diagnostic::Severity;

use super::ast::*;
use super::diag::LanguageErrors;
use super::parser::*;

#[derive(PartialEq, Eq, Clone)]
pub enum Pattern {
    LeftRecursive(Vec<Regex>),
    OperatorPrecedence(Vec<Regex>),
    UnconditionalForwarding,
    ConditionalForwarding,
    RightRecursiveForwarding(Vec<Regex>),
    MaybeEmpty,
}

impl std::fmt::Debug for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Pattern::LeftRecursive(_) => "LeftRecursive",
                Pattern::OperatorPrecedence(_) => "OperatorPrecedence",
                Pattern::UnconditionalForwarding => "UnconditionalForwarding",
                Pattern::ConditionalForwarding => "ConditionalForwarding",
                Pattern::RightRecursiveForwarding(_) => "RightRecursiveForwarding",
                Pattern::MaybeEmpty => "MaybeEmpty",
            }
        )
    }
}

pub struct SemanticPass;

impl SemanticPass {
    pub fn run<'a>(cst: &'a Cst, diags: &mut Vec<Diagnostic>) -> SemanticData<'a> {
        let mut sema = SemanticData::default();
        GeneralCheck::new().run(cst, diags, &mut sema);
        if !diags.iter().any(|d| d.severity == Severity::Error) {
            LL1Validator::run(cst, diags, &mut sema);
            UsageValidator::run(cst, diags, &mut sema);
            if !diags.iter().any(|d| d.severity == Severity::Error) {
                RecoverySetGenerator::new().run(cst, &mut sema);
            }
        }
        sema
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Copy, Default)]
pub struct TokenName<'a>(pub &'a str);

impl<'a> std::fmt::Debug for TokenName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Default)]
pub struct SemanticData<'a> {
    pub decl_bindings: HashMap<NodeRef, NodeRef>,
    pub patterns: HashMap<RuleDecl, Pattern>,
    pub right_associative: HashSet<&'a str>,
    pub skipped: BTreeSet<TokenDecl>,
    pub start: Option<RuleDecl>,
    pub predicates: BTreeMap<NodeRef, (&'a str, &'a str)>,
    pub actions: BTreeMap<NodeRef, (&'a str, &'a str)>,
    pub rule_bindings: BTreeSet<&'a str>,
    pub first_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub follow_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub predict_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub recovery_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub left_rec_local_follow_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub used: HashSet<NodeRef>,
    pub has_rule_binding: HashSet<RuleDecl>,
}

#[derive(Default)]
struct GeneralCheck<'a> {
    symbol_table: std::collections::HashMap<&'a str, NodeRef>,
    current_rule: Option<RuleDecl>,
}

impl<'a> GeneralCheck<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    fn bind_symbol(
        &mut self,
        cst: &'a Cst,
        name: &'a str,
        binding: &str,
        syntax: NodeRef,
        diags: &mut Vec<Diagnostic>,
    ) {
        if let Some(old) = self.symbol_table.insert(name, syntax) {
            let span = if let Some(span) = cst.get_span(syntax) {
                span
            } else {
                return;
            };
            let old_span = if let Some(span) = cst.get_span(old) {
                span
            } else {
                return;
            };
            diags.push(Diagnostic::redefinition(&span, binding, &old_span));
        }
    }
    fn get_symbol_binding(
        &mut self,
        name: &'a str,
        rule_binding: bool,
        span: Span,
        diags: &mut Vec<Diagnostic>,
    ) -> Option<NodeRef> {
        self.symbol_table
            .get(name)
            .or_else(|| {
                if rule_binding {
                    diags.push(Diagnostic::undefined_rule(&span, name));
                } else {
                    diags.push(Diagnostic::undefined_token(&span, name));
                }
                None
            })
            .copied()
    }
    fn run(&mut self, cst: &'a Cst, diags: &mut Vec<Diagnostic>, sema: &mut SemanticData<'a>) {
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            file.rule_decls(cst)
                .for_each(|decl| self.bind_rule_decl(cst, decl, diags));

            file.token_decls(cst)
                .for_each(|decl| self.check_token_decl(cst, decl, diags));
            file.right_decls(cst)
                .for_each(|decl| self.check_right_decl(cst, decl, diags, sema));
            file.skip_decls(cst)
                .for_each(|decl| self.check_skip_decl(cst, decl, diags, sema));
            file.rule_decls(cst)
                .for_each(|decl| self.check_rule_decl(cst, decl, diags, sema));
            file.start_decls(cst)
                .for_each(|decl| self.check_start_decl(cst, decl, diags, sema));

            file.rule_decls(cst).for_each(|decl| {
                if let Some(start) = sema.start {
                    if decl == start {
                        return;
                    }
                }
                let _ = self.check_binary_precedence(cst, sema, decl)
                    || self.check_left_recursive(cst, sema, decl)
                    || self.check_right_recursive(cst, sema, decl)
                    || self.check_forwarding_or_empty(cst, sema, decl);
            });
        }
        if let Some(start) = sema.start {
            for (name, rule) in sema.decl_bindings.iter() {
                if *rule == start.syntax() {
                    diags.push(Diagnostic::reference_start_rule(
                        &cst.get_span(*name).unwrap(),
                    ));
                }
            }
        } else {
            diags.push(Diagnostic::missing_start_rule());
        }
    }
    fn check_token_decl(&mut self, cst: &'a Cst, decl: TokenDecl, diags: &mut Vec<Diagnostic>) {
        if let Some((name, name_span)) = decl.name(cst) {
            if name == "EOF" {
                diags.push(Diagnostic::predefined_token_name(&name_span));
                return;
            }
            self.bind_symbol(cst, name, "token", decl.syntax(), diags);
            if name.starts_with(|c: char| c.is_lowercase()) {
                diags.push(Diagnostic::lowercase_token(&name_span, name));
            }
        }
        if let Some((name, _)) = decl.symbol(cst) {
            self.bind_symbol(cst, name, "token", decl.syntax(), diags);
        }
    }
    fn bind_rule_decl(&mut self, cst: &'a Cst, decl: RuleDecl, diags: &mut Vec<Diagnostic>) {
        if let Some((name, name_span)) = decl.name(cst) {
            self.bind_symbol(cst, name, "rule", decl.syntax(), diags);
            if name.starts_with(|c: char| c.is_uppercase()) {
                diags.push(Diagnostic::uppercase_rule(&name_span, name));
            }
        }
    }
    fn check_rule_decl(
        &mut self,
        cst: &'a Cst,
        decl: RuleDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        self.current_rule = Some(decl);
        decl.regex(cst)
            .inspect(|regex| self.check_regex(cst, *regex, diags, sema, false, false, false));
    }
    fn check_start_decl(
        &mut self,
        cst: &'a Cst,
        start_decl: StartDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        if let Some(rule_decl) = start_decl
            .rule_name(cst)
            .and_then(|(name, name_span)| self.get_symbol_binding(name, true, name_span, diags))
            .and_then(|node| RuleDecl::cast(cst, node))
        {
            sema.start = Some(rule_decl);
        }
    }
    fn check_right_decl(
        &mut self,
        cst: &'a Cst,
        right_decl: RightDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        right_decl.token_names(cst, |(name, name_span)| {
            if let Some(node) = self.get_symbol_binding(name, false, name_span.clone(), diags) {
                if let Some(token_decl) = TokenDecl::cast(cst, node) {
                    if let Some((name, _)) = token_decl.name(cst) {
                        if sema.right_associative.contains(&name) {
                            diags.push(Diagnostic::redefine_as_right(&name_span));
                        } else {
                            sema.right_associative.insert(name);
                        }
                    }
                } else {
                    diags.push(Diagnostic::expected_token(&name_span));
                }
            }
        });
    }
    fn check_skip_decl(
        &mut self,
        cst: &'a Cst,
        skip_decl: SkipDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        skip_decl.token_names(cst, |(name, name_span)| {
            if let Some(node) = self.get_symbol_binding(name, false, name_span.clone(), diags) {
                if let Some(token_decl) = TokenDecl::cast(cst, node) {
                    if sema.skipped.contains(&token_decl) {
                        diags.push(Diagnostic::redefine_as_skipped(&name_span));
                    } else {
                        sema.skipped.insert(token_decl);
                    }
                } else {
                    diags.push(Diagnostic::expected_token(&name_span));
                }
            }
        });
    }
    #[allow(clippy::too_many_arguments)]
    fn check_regex(
        &mut self,
        cst: &'a Cst,
        regex: Regex,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
        in_alt: bool,
        in_loop: bool,
        at_concat_end: bool,
    ) {
        match regex {
            Regex::Alternation(regex) => regex
                .operands(cst)
                .for_each(|regex| self.check_regex(cst, regex, diags, sema, true, false, false)),
            Regex::Concat(regex) => {
                let mut in_alt = in_alt;
                let mut in_loop = in_loop;
                let mut ops = regex.operands(cst).peekable();
                while let Some(regex) = ops.next() {
                    self.check_regex(
                        cst,
                        regex,
                        diags,
                        sema,
                        in_alt,
                        in_loop,
                        ops.peek().is_none(),
                    );
                    in_alt = false;
                    in_loop = false;
                }
            }
            Regex::Paren(regex) => {
                regex.inner(cst).inspect(|regex| {
                    self.check_regex(cst, *regex, diags, sema, false, in_loop, false)
                });
            }
            Regex::Optional(regex) => {
                regex.operand(cst).inspect(|regex| {
                    self.check_regex(cst, *regex, diags, sema, false, true, false)
                });
            }
            Regex::Star(regex) => {
                regex.operand(cst).inspect(|regex| {
                    self.check_regex(cst, *regex, diags, sema, false, true, false)
                });
            }
            Regex::Plus(regex) => {
                regex.operand(cst).inspect(|regex| {
                    self.check_regex(cst, *regex, diags, sema, false, true, false)
                });
            }
            Regex::Name(regex) => {
                if let Some((name, name_span)) = regex.value(cst) {
                    let rule_binding = name.starts_with(|c: char| c.is_lowercase());
                    if let Some(decl) =
                        self.get_symbol_binding(name, rule_binding, name_span.clone(), diags)
                    {
                        if let Some(token) = TokenDecl::cast(cst, decl) {
                            if sema.skipped.contains(&token) {
                                diags.push(Diagnostic::used_skipped(&name_span));
                            }
                        }
                        sema.decl_bindings.insert(regex.syntax(), decl);
                    }
                }
            }
            Regex::Symbol(regex) => {
                if let Some((name, name_span)) = regex.value(cst) {
                    if let Some(decl) =
                        self.get_symbol_binding(name, false, name_span.clone(), diags)
                    {
                        if let Some(token) = TokenDecl::cast(cst, decl) {
                            if sema.skipped.contains(&token) {
                                diags.push(Diagnostic::used_skipped(&name_span));
                            }
                        }
                        sema.decl_bindings.insert(regex.syntax(), decl);
                    }
                }
            }
            Regex::Predicate(regex) => {
                if let Some((value, value_span)) = regex.value(cst) {
                    if !in_alt && !in_loop {
                        diags.push(Diagnostic::invalid_predicate_pos(&value_span));
                    }
                    if let Some(rule_name) = self
                        .current_rule
                        .and_then(|rule| rule.name(cst).map(|(name, _)| name))
                    {
                        sema.predicates
                            .insert(regex.syntax(), (rule_name, &value[1..]));
                    }
                }
            }
            Regex::Action(regex) => {
                if let Some((value, _)) = regex.value(cst) {
                    if let Some(rule_name) = self
                        .current_rule
                        .and_then(|rule| rule.name(cst).map(|(name, _)| name))
                    {
                        sema.actions
                            .insert(regex.syntax(), (rule_name, &value[1..]));
                    }
                }
            }
            Regex::Binding(regex) => {
                if !at_concat_end {
                    diags.push(Diagnostic::invalid_binding_pos(&regex.span(cst)));
                }
                if let Some((name, name_span)) = regex.value(cst) {
                    let name = &name[1..];
                    if !name.is_empty() {
                        if name.starts_with(|c: char| c.is_uppercase()) {
                            diags.push(Diagnostic::uppercase_rule(&name_span, name));
                        }
                        if let Some(rule) = self.current_rule {
                            sema.has_rule_binding.insert(rule);
                        }
                        sema.rule_bindings.insert(name);
                    }
                }
            }
            Regex::OpenNode(_) => {}
            Regex::CloseNode(regex) => {
                if let Some(name) = regex.node_name(cst) {
                    if !name.is_empty() {
                        if name.starts_with(|c: char| c.is_uppercase()) {
                            diags.push(Diagnostic::uppercase_rule(&regex.span(cst), name));
                        }
                        sema.rule_bindings.insert(name);
                    }
                }
            }
        }
    }
    fn name_references_rule(
        &self,
        cst: &'a Cst,
        sema: &SemanticData,
        rule: RuleDecl,
        regex: Regex,
    ) -> bool {
        if let Regex::Name(name) = regex {
            sema.decl_bindings
                .get(&name.syntax())
                .and_then(|n| RuleDecl::cast(cst, *n))
                .map(|r| r.syntax() == rule.syntax())
                .unwrap_or(false)
        } else {
            false
        }
    }
    fn check_binary_precedence(
        &self,
        cst: &'a Cst,
        sema: &mut SemanticData,
        rule: RuleDecl,
    ) -> bool {
        let mut branches = vec![];
        fn is_operator(sema: &SemanticData, cst: &Cst, op: &Regex, outer: bool) -> bool {
            match op {
                Regex::Name(op) => sema
                    .decl_bindings
                    .get(&op.syntax())
                    .and_then(|n| TokenDecl::cast(cst, *n))
                    .is_some(),
                Regex::Symbol(_) => true,
                Regex::Paren(op) => op
                    .inner(cst)
                    .map_or(false, |op| is_operator(sema, cst, &op, outer)),
                Regex::Alternation(op) if outer => {
                    let mut res = true;
                    op.operands(cst).for_each(|n| {
                        res &= is_operator(sema, cst, &n, false);
                    });
                    res
                }
                _ => false,
            }
        }

        if let Some(Regex::Alternation(alt)) = rule.regex(cst) {
            let mut count = 0;
            for alt_op in alt.operands(cst) {
                count += 1;
                if let Regex::Concat(concat) = alt_op {
                    let mut it = concat.operands(cst);
                    let (lhs, op, rhs) = (it.next(), it.next(), it.next());
                    if lhs.is_none() || op.is_none() || rhs.is_none() || it.next().is_some() {
                        // not a binary expression
                        return false;
                    }
                    let (lhs, op, rhs) = (lhs.unwrap(), op.unwrap(), rhs.unwrap());
                    if !self.name_references_rule(cst, sema, rule, lhs)
                        || !is_operator(sema, cst, &op, true)
                        || !self.name_references_rule(cst, sema, rule, rhs)
                    {
                        // not a binary expression
                        return false;
                    }
                    branches.push(alt_op);
                }
            }
            if branches.len() != count - 1 {
                // not a singular exit branch
                return false;
            }
        }
        if !branches.is_empty() {
            sema.patterns
                .insert(rule, Pattern::OperatorPrecedence(branches));
            return true;
        }
        false
    }
    fn check_left_recursive(&self, cst: &'a Cst, sema: &mut SemanticData, rule: RuleDecl) -> bool {
        let mut branches = vec![];
        if let Some(Regex::Alternation(alt)) = rule.regex(cst) {
            for alt_op in alt.operands(cst) {
                if let Regex::Concat(concat) = alt_op {
                    if let Some(concat_op) = concat
                        .operands(cst)
                        .find(|op| !matches!(op, Regex::Predicate(_)))
                    {
                        if self.name_references_rule(cst, sema, rule, concat_op) {
                            branches.push(alt_op)
                        }
                    }
                }
            }
        }

        if !branches.is_empty() {
            sema.patterns.insert(rule, Pattern::LeftRecursive(branches));
            return true;
        }
        false
    }
    fn check_right_recursive(&self, cst: &'a Cst, sema: &mut SemanticData, rule: RuleDecl) -> bool {
        let mut branches = vec![];
        let mut is_right_recursive = false;
        if let Some(Regex::Alternation(alt)) = rule.regex(cst) {
            for op in alt.operands(cst) {
                match op {
                    Regex::Concat(concat) => {
                        if let Some(op) = concat
                            .operands(cst)
                            .filter(|op| !matches!(op, Regex::Binding(_)))
                            .last()
                        {
                            if self.name_references_rule(cst, sema, rule, op) {
                                // at least one recursive branch
                                is_right_recursive = true;
                            }
                        }
                    }
                    Regex::Name(ref name) => {
                        if sema
                            .decl_bindings
                            .get(&name.syntax())
                            .and_then(|n| RuleDecl::cast(cst, *n))
                            .is_some()
                        {
                            // exit branch
                            branches.push(op)
                        }
                    }
                    _ => {}
                }
            }
        }
        if is_right_recursive && !branches.is_empty() {
            sema.patterns
                .insert(rule, Pattern::RightRecursiveForwarding(branches));
            return true;
        }
        false
    }
    fn check_forwarding_or_empty(
        &self,
        cst: &'a Cst,
        sema: &mut SemanticData,
        rule: RuleDecl,
    ) -> bool {
        let mut pat = None;
        fn is_rule_ref(sema: &SemanticData, cst: &Cst, regex: Regex) -> bool {
            match regex {
                Regex::Paren(paren) => paren
                    .inner(cst)
                    .map_or(false, |inner| is_rule_ref(sema, cst, inner)),
                Regex::Alternation(alt) => {
                    for op in alt.operands(cst) {
                        if !is_rule_ref(sema, cst, op) {
                            return false;
                        }
                    }
                    true
                }
                Regex::Concat(concat) => {
                    let mut it = concat.operands(cst);
                    if !it
                        .next()
                        .map_or(false, |op| matches!(op, Regex::Predicate(_)))
                    {
                        return false;
                    }
                    is_rule_ref(sema, cst, it.next().unwrap())
                }
                Regex::Name(name) => sema
                    .decl_bindings
                    .get(&name.syntax())
                    .and_then(|n| RuleDecl::cast(cst, *n))
                    .is_some(),
                _ => false,
            }
        }
        let is_maybe_empty = |regex| matches!(regex, Regex::Star(_) | Regex::Optional(_));

        if let Some(regex) = rule.regex(cst) {
            match regex {
                Regex::Concat(concat) => {
                    let mut it = concat.operands(cst);
                    if !it.next().map_or(false, |op| is_rule_ref(sema, cst, op)) {
                        return false;
                    }
                    for op in it {
                        if is_maybe_empty(op) {
                            pat = Some(Pattern::ConditionalForwarding);
                        } else {
                            return false;
                        }
                    }
                }
                _ => {
                    if is_rule_ref(sema, cst, regex) {
                        pat = Some(Pattern::UnconditionalForwarding);
                    } else if is_maybe_empty(regex) {
                        pat = Some(Pattern::MaybeEmpty);
                    }
                }
            }
        }
        if let Some(pat) = pat {
            sema.patterns.insert(rule, pat);
            return true;
        }
        false
    }
}

struct LL1Validator;

impl<'a> LL1Validator {
    /// Validates that the grammar is an LL(1) grammar.
    fn run(cst: &'a Cst, diags: &mut Vec<Diagnostic>, sema: &mut SemanticData<'a>) {
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            Self::calc_first(cst, sema, file);
            Self::calc_follow(cst, sema, file);
            Self::calc_predict(sema);
            Self::check(cst, sema, diags, file);
        }
    }

    /// Calculates the first set for each grammar rule.
    fn calc_first(cst: &'a Cst, sema: &mut SemanticData<'a>, file: File) {
        // Iterates until there are no more changes in the first sets
        let mut change = true;
        while change {
            change = false;
            for rule in file.rule_decls(cst) {
                if let Some(regex) = rule.regex(cst) {
                    Self::calc_first_regex(cst, sema, regex, &mut change);
                }
            }
        }
    }

    /// Calculates the first set for each regular expression within a rule.
    fn calc_first_regex(
        cst: &'a Cst,
        sema: &mut SemanticData<'a>,
        regex: Regex,
        change: &mut bool,
    ) {
        let entry = sema.first_sets.entry(regex.syntax()).or_default();
        let size = entry.len();
        match regex {
            Regex::Name(name) => {
                let decl = sema.decl_bindings.get(&name.syntax());
                if let Some(rule) = decl.and_then(|decl| RuleDecl::cast(cst, *decl)) {
                    if let Some(rule_regex) = rule.regex(cst) {
                        let rule_first = sema
                            .first_sets
                            .entry(rule_regex.syntax())
                            .or_default()
                            .clone();
                        sema.first_sets
                            .get_mut(&regex.syntax())
                            .unwrap()
                            .extend(rule_first);
                    } else {
                        entry.insert(TokenName("ɛ"));
                    }
                } else if let Some((name, _)) = decl
                    .and_then(|decl| TokenDecl::cast(cst, *decl))
                    .and_then(|token| token.name(cst))
                {
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .insert(TokenName(name));
                }
            }
            Regex::Symbol(sym) => {
                let decl = sema.decl_bindings.get(&sym.syntax());
                if let Some((name, _)) = decl
                    .and_then(|decl| TokenDecl::cast(cst, *decl))
                    .and_then(|token| token.name(cst))
                {
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .insert(TokenName(name));
                }
            }
            Regex::Concat(concat) => {
                let mut use_next = true;
                for op in concat.operands(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    // only add next first set if there was an epsilon
                    if use_next {
                        let op_first = sema.first_sets[&op.syntax()].clone();
                        use_next = op_first.contains(&TokenName("ɛ"));
                        let first = sema.first_sets.get_mut(&regex.syntax()).unwrap();
                        first.extend(op_first.into_iter());
                        first.remove(&TokenName("ɛ"));
                    }
                }
                if use_next {
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .insert(TokenName("ɛ"));
                }
            }
            Regex::Alternation(alt) => {
                for op in alt.operands(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .extend(op_first.into_iter());
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    let first = sema.first_sets.get_mut(&regex.syntax()).unwrap();
                    first.extend(op_first);
                    first.insert(TokenName("ɛ"));
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    let first = sema.first_sets.get_mut(&regex.syntax()).unwrap();
                    first.extend(op_first);
                    first.insert(TokenName("ɛ"));
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .extend(op_first);
                }
            }
            Regex::Paren(paren) => {
                if let Some(op) = paren.inner(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .extend(op_first);
                }
            }
            _ => {
                entry.insert(TokenName("ɛ"));
            }
        };
        *change |= sema.first_sets[&regex.syntax()].len() != size;
    }

    /// Calculates the follow set for each grammar rule.
    fn calc_follow(cst: &'a Cst, sema: &mut SemanticData<'a>, file: File) {
        if let Some(start_rule_regex) = sema.start.and_then(|start| start.regex(cst)) {
            sema.follow_sets
                .entry(start_rule_regex.syntax())
                .or_default()
                .insert(TokenName("EOF"));
        }
        // Iterates until there are no more changes in the follow sets
        let mut change = true;
        while change {
            change = false;
            for rule in file.rule_decls(cst) {
                if let Some(regex) = rule.regex(cst) {
                    sema.follow_sets.entry(regex.syntax()).or_default();
                    Self::calc_follow_regex(cst, sema, regex, regex, &mut change);
                }
            }
        }
    }
    /// Calculates the follow set for each regular expression within a rule.
    fn calc_follow_regex(
        cst: &Cst,
        sema: &mut SemanticData<'a>,
        regex: Regex,
        rule_regex: Regex,
        change: &mut bool,
    ) {
        match regex {
            Regex::Name(name) => {
                let decl = sema.decl_bindings.get(&name.syntax());
                if let Some(name_rule_regex) = decl
                    .and_then(|decl| RuleDecl::cast(cst, *decl))
                    .and_then(|rule| rule.regex(cst))
                {
                    let size = sema
                        .follow_sets
                        .entry(name_rule_regex.syntax())
                        .or_default()
                        .len();
                    let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                    let left_rec_local_follow = sema
                        .left_rec_local_follow_sets
                        .entry(name_rule_regex.syntax())
                        .or_default();
                    if rule_regex != name_rule_regex {
                        left_rec_local_follow.extend(follow.clone());
                    }
                    sema.follow_sets
                        .get_mut(&name_rule_regex.syntax())
                        .unwrap()
                        .extend(follow);
                    *change |= size != sema.follow_sets[&name_rule_regex.syntax()].len();
                }
            }
            Regex::Concat(concat) => {
                let mut follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                for op in concat.operands(cst).collect::<Vec<_>>().into_iter().rev() {
                    sema.follow_sets
                        .entry(op.syntax())
                        .or_default()
                        .extend(follow.iter());
                    let op_first = &sema.first_sets[&op.syntax()];
                    if op_first.contains(&TokenName("ɛ")) {
                        follow.extend(op_first.iter());
                        follow.remove(&TokenName("ɛ"));
                    } else {
                        follow.clone_from(op_first);
                    }
                    Self::calc_follow_regex(cst, sema, op, rule_regex, change);
                }
            }
            Regex::Alternation(alt) => {
                let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                for op in alt.operands(cst) {
                    sema.follow_sets
                        .entry(op.syntax())
                        .or_default()
                        .extend(follow.iter());
                    Self::calc_follow_regex(cst, sema, op, rule_regex, change);
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                    let op_first = &sema.first_sets[&op.syntax()];
                    let op_follow = sema.follow_sets.entry(op.syntax()).or_default();
                    op_follow.extend(op_first.iter());
                    op_follow.remove(&TokenName("ɛ"));
                    op_follow.extend(follow);
                    Self::calc_follow_regex(cst, sema, op, rule_regex, change);
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    let first = &sema.first_sets[&regex.syntax()];
                    let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                    let op_follow = sema.follow_sets.entry(op.syntax()).or_default();
                    op_follow.extend(first.iter());
                    op_follow.extend(follow);
                    Self::calc_follow_regex(cst, sema, op, rule_regex, change);
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                    sema.follow_sets
                        .entry(op.syntax())
                        .or_default()
                        .extend(follow);
                    Self::calc_follow_regex(cst, sema, op, rule_regex, change);
                }
            }
            Regex::Paren(paren) => {
                if let Some(inner) = paren.inner(cst) {
                    let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                    sema.follow_sets
                        .entry(inner.syntax())
                        .or_default()
                        .extend(follow);
                    Self::calc_follow_regex(cst, sema, inner, rule_regex, change);
                }
            }
            _ => {}
        };
    }

    fn calc_predict(sema: &mut SemanticData<'a>) {
        for (node, first) in sema.first_sets.iter() {
            let mut set = first.clone();
            if set.contains(&TokenName("ɛ")) {
                set.remove(&TokenName("ɛ"));
                set.extend(sema.follow_sets[node].iter());
            }
            sema.predict_sets.insert(*node, set);
        }
    }

    /// Checks if LL(1) condition holds for the all regexes.
    fn check(cst: &Cst, sema: &SemanticData<'a>, diags: &mut Vec<Diagnostic>, file: File) {
        for rule in file.rule_decls(cst) {
            let mut left_recursive = &vec![];
            if let Some(
                Pattern::LeftRecursive(ref branches) | Pattern::OperatorPrecedence(ref branches),
            ) = sema.patterns.get(&rule)
            {
                left_recursive = branches;
            }
            if let Some(regex) = rule.regex(cst) {
                Self::check_regex(cst, sema, diags, regex, rule, left_recursive);
            }
        }
    }

    fn has_predicate(cst: &Cst, regex: Regex) -> bool {
        match regex {
            Regex::Concat(concat) => {
                matches!(concat.operands(cst).next(), Some(Regex::Predicate(_)))
            }
            Regex::Paren(paren) => paren
                .inner(cst)
                .map_or(false, |inner| Self::has_predicate(cst, inner)),
            _ => false,
        }
    }

    fn skip_first(cst: &Cst, op: Regex) -> Regex {
        if let Regex::Concat(concat) = op {
            concat
                .operands(cst)
                .filter(|op| !matches!(op, Regex::Predicate(_)))
                .nth(1)
                .unwrap()
        } else {
            unreachable!()
        }
    }

    fn check_intersection(
        cst: &Cst,
        sema: &SemanticData<'a>,
        diags: &mut Vec<Diagnostic>,
        op: Regex,
        branches: impl Iterator<Item = Regex>,
        i: usize,
        left_rec: bool,
    ) {
        let prediction = &sema.predict_sets[&op.syntax()];
        let mut related = vec![];
        for op in branches.skip(i + 1) {
            let op = if left_rec {
                Self::skip_first(cst, op)
            } else {
                op
            };
            let other_prediction = &sema.predict_sets[&op.syntax()];
            let intersection = prediction
                .intersection(other_prediction)
                .copied()
                .collect::<BTreeSet<_>>();
            if !intersection.is_empty() {
                let set = format!("with token set: {:?}", intersection);
                related.push((cst.get_span(op.syntax()).unwrap().clone(), set));
            }
        }
        if !related.is_empty() {
            if left_rec {
                diags.push(Diagnostic::ll1_conflict_left_rec(&op.span(cst), related));
            } else {
                diags.push(Diagnostic::ll1_conflict_alt(&op.span(cst), related));
            }
        }
    }

    /// Checks if LL(1) condition holds for the regex.
    fn check_regex(
        cst: &Cst,
        sema: &SemanticData<'a>,
        diags: &mut Vec<Diagnostic>,
        regex: Regex,
        rule: RuleDecl,
        left_recursive: &[Regex],
    ) {
        match regex {
            Regex::Alternation(alt) => {
                for i in 0..left_recursive.len() {
                    let op = Self::skip_first(cst, left_recursive[i]);

                    if !Self::has_predicate(cst, left_recursive[i]) {
                        let prediction = &sema.predict_sets[&op.syntax()];
                        let local_follow = &sema.left_rec_local_follow_sets[&alt.syntax()];
                        let intersection = prediction
                            .intersection(local_follow)
                            .copied()
                            .collect::<BTreeSet<_>>();
                        if !intersection.is_empty() {
                            let set = format!("with token set: {:?}", intersection);
                            let related = vec![(rule.name(cst).unwrap_or_default().1, set)];
                            diags.push(Diagnostic::ll1_conflict_left_rec(
                                &cst.get_span(op.syntax()).unwrap_or_default(),
                                related,
                            ));
                        }
                    }

                    Self::check_intersection(
                        cst,
                        sema,
                        diags,
                        op,
                        left_recursive.iter().copied(),
                        i,
                        true,
                    );
                }
                let is_exit_branch =
                    |op: &Regex| !Self::has_predicate(cst, *op) && !left_recursive.contains(op);

                for (i, op) in alt.operands(cst).enumerate() {
                    if !is_exit_branch(&op) {
                        continue;
                    }
                    let operands = alt.operands(cst).filter(|op| !left_recursive.contains(op));
                    Self::check_intersection(cst, sema, diags, op, operands, i, false);
                }
                for op in alt.operands(cst) {
                    Self::check_regex(cst, sema, diags, op, rule, &[]);
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    let intersection = sema.follow_sets[&regex.syntax()]
                        .intersection(&sema.predict_sets[&op.syntax()])
                        .copied()
                        .collect::<BTreeSet<_>>();
                    if !Self::has_predicate(cst, op) && !intersection.is_empty() {
                        let set = format!("with token set: {:?}", intersection);
                        diags.push(Diagnostic::ll1_conflict_rep(&regex.span(cst), set));
                    }
                    Self::check_regex(cst, sema, diags, op, rule, &[]);
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    let intersection = sema.follow_sets[&regex.syntax()]
                        .intersection(&sema.predict_sets[&op.syntax()])
                        .copied()
                        .collect::<BTreeSet<_>>();
                    if !Self::has_predicate(cst, op) && !intersection.is_empty() {
                        let set = format!("with token set: {:?}", intersection);
                        diags.push(Diagnostic::ll1_conflict_rep(&regex.span(cst), set));
                    }
                    Self::check_regex(cst, sema, diags, op, rule, &[]);
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    let intersection = sema.follow_sets[&regex.syntax()]
                        .intersection(&sema.predict_sets[&op.syntax()])
                        .copied()
                        .collect::<BTreeSet<_>>();
                    if !Self::has_predicate(cst, op) && !intersection.is_empty() {
                        let set = format!("with token set: {:?}", intersection);
                        diags.push(Diagnostic::ll1_conflict_opt(&regex.span(cst), set));
                    }
                    Self::check_regex(cst, sema, diags, op, rule, &[]);
                }
            }
            Regex::Name(name) => {
                if sema
                    .first_sets
                    .get(&name.syntax())
                    .map_or(false, |first| first.is_empty())
                {
                    diags.push(Diagnostic::consume_tokens(&regex.span(cst)));
                }
            }
            Regex::Concat(concat) => {
                for op in concat.operands(cst) {
                    Self::check_regex(cst, sema, diags, op, rule, &[]);
                }
            }
            Regex::Paren(paren) => {
                if let Some(inner) = paren.inner(cst) {
                    Self::check_regex(cst, sema, diags, inner, rule, &[]);
                }
            }
            _ => {}
        };
    }
}

struct UsageValidator;

impl UsageValidator {
    fn run(cst: &Cst, diags: &mut Vec<Diagnostic>, sema: &mut SemanticData) {
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            if let Some(rule) = sema.start {
                sema.used.insert(rule.syntax());
            }
            for token in sema.skipped.iter() {
                sema.used.insert(token.syntax());
            }
            let mut change = true;
            while change {
                let count = sema.used.len();
                for rule in file.rule_decls(cst) {
                    if sema.used.contains(&rule.syntax()) {
                        if let Some(regex) = rule.regex(cst) {
                            Self::set_regex(cst, sema, regex);
                        }
                    }
                }
                change = count != sema.used.len();
            }
            for rule in file.rule_decls(cst) {
                if !sema.used.contains(&rule.syntax()) {
                    diags.push(Diagnostic::unused_rule(&rule.span(cst)));
                }
            }
            for token in file.token_decls(cst) {
                if !sema.used.contains(&token.syntax()) {
                    diags.push(Diagnostic::unused_token(&token.span(cst)));
                }
            }
        }
    }
    fn set_regex(cst: &Cst, sema: &mut SemanticData, regex: Regex) {
        match regex {
            Regex::Alternation(alt) => alt
                .operands(cst)
                .for_each(|op| Self::set_regex(cst, sema, op)),
            Regex::Concat(concat) => concat
                .operands(cst)
                .for_each(|op| Self::set_regex(cst, sema, op)),
            Regex::Paren(paren) => {
                if let Some(inner) = paren.inner(cst) {
                    Self::set_regex(cst, sema, inner)
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    Self::set_regex(cst, sema, op)
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    Self::set_regex(cst, sema, op)
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    Self::set_regex(cst, sema, op)
                }
            }
            Regex::Name(name) => {
                sema.decl_bindings
                    .get(&name.syntax())
                    .map(|node| sema.used.insert(*node));
            }
            Regex::Symbol(sym) => {
                sema.decl_bindings
                    .get(&sym.syntax())
                    .map(|node| sema.used.insert(*node));
            }
            Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Binding(_)
            | Regex::OpenNode(_)
            | Regex::CloseNode(_) => {}
        }
    }
}

#[derive(Default)]
struct RecoverySetGenerator {
    dom: HashMap<Regex, HashSet<Regex>>,
    pred: HashMap<Regex, HashSet<Regex>>,
}
impl RecoverySetGenerator {
    fn new() -> Self {
        Self::default()
    }
    fn run(&mut self, cst: &Cst, sema: &mut SemanticData) {
        let file = if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            file
        } else {
            return;
        };

        let start = if let Some(start_regex) = sema.start.and_then(|start| start.regex(cst)) {
            start_regex
        } else {
            return;
        };
        for rule in file.rule_decls(cst) {
            if sema.used.contains(&rule.syntax()) {
                if let Some(regex) = rule.regex(cst) {
                    self.set_regex_pred(cst, sema, regex);
                }
            }
        }

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

        // calculate recovery set for loops
        if let Regex::Star(_) | Regex::Plus(_) = start {
            sema.recovery_sets.entry(start.syntax()).or_default();
        }
        for regex in nodes_no_start.iter() {
            let op = if let Regex::Star(star) = regex {
                star.operand(cst).unwrap()
            } else if let Regex::Plus(plus) = regex {
                plus.operand(cst).unwrap()
            } else {
                continue;
            };
            let op_follow = &sema.follow_sets[&op.syntax()];
            for dom in self.dom[regex].iter() {
                let dom_follow = &sema.follow_sets[&dom.syntax()];
                sema.recovery_sets
                    .entry(regex.syntax())
                    .or_default()
                    .extend(dom_follow);
            }
            for sym in op_follow.iter() {
                sema.recovery_sets
                    .entry(regex.syntax())
                    .or_default()
                    .remove(sym);
            }
        }
    }

    fn add_pred(&mut self, r: Regex, p: Regex) {
        if let Some(pred) = self.pred.get_mut(&r) {
            pred.insert(p);
        } else {
            self.pred.insert(r, HashSet::from_iter([p]));
        }
    }

    fn set_regex_pred(&mut self, cst: &Cst, sema: &SemanticData, regex: Regex) {
        match regex {
            Regex::Name(name) => {
                if let Some(rule_regex) = sema
                    .decl_bindings
                    .get(&name.syntax())
                    .and_then(|n| RuleDecl::cast(cst, *n))
                    .and_then(|rule| rule.regex(cst))
                {
                    self.add_pred(rule_regex, regex);
                }
            }
            Regex::Concat(concat) => {
                for op in concat.operands(cst) {
                    self.add_pred(op, regex);
                    self.set_regex_pred(cst, sema, op);
                }
            }
            Regex::Alternation(alt) => {
                for op in alt.operands(cst) {
                    self.add_pred(op, regex);
                    self.set_regex_pred(cst, sema, op);
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    self.add_pred(op, regex);
                    self.set_regex_pred(cst, sema, op);
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    self.add_pred(op, regex);
                    self.set_regex_pred(cst, sema, op);
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    self.add_pred(op, regex);
                    self.set_regex_pred(cst, sema, op);
                }
            }
            Regex::Paren(paren) => {
                if let Some(inner) = paren.inner(cst) {
                    self.add_pred(inner, regex);
                    self.set_regex_pred(cst, sema, inner);
                }
            }
            Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Binding(_)
            | Regex::OpenNode(_)
            | Regex::CloseNode(_) => {}
        }
    }
}
