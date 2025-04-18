use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use codespan_reporting::diagnostic::Severity;

use super::ast::*;
use super::diag::LanguageErrors;
use super::parser::*;

pub struct SemanticPass;

impl SemanticPass {
    pub fn run<'a>(cst: &'a Cst, diags: &mut Vec<Diagnostic>) -> SemanticData<'a> {
        let mut sema = SemanticData::default();
        GeneralCheck::new().run(cst, diags, &mut sema);
        if !diags.iter().any(|d| d.severity == Severity::Error) {
            OrderedChoiceValidator::run(cst, diags, &mut sema);
            LL1Validator::run(cst, diags, &mut sema);
            UsageValidator::run(cst, diags, &mut sema);
            OperatorValidator::run(cst, diags, &mut sema);
            if !diags.iter().any(|d| d.severity == Severity::Error) {
                RecoverySetGenerator::new().run(cst, &mut sema);
            }
        }
        sema
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd, Clone, Copy, Default)]
pub struct TokenName<'a>(pub &'a str);

impl std::fmt::Debug for TokenName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RuleNodeElision {
    None,
    Unconditional,
    Conditional,
}
impl RuleNodeElision {
    fn alt(self, other: Self) -> Self {
        match (self, other) {
            (RuleNodeElision::None, RuleNodeElision::None) => RuleNodeElision::None,
            (RuleNodeElision::None, _) => RuleNodeElision::Conditional,
            (RuleNodeElision::Unconditional, RuleNodeElision::Unconditional) => {
                RuleNodeElision::Unconditional
            }
            (RuleNodeElision::Unconditional, _) => RuleNodeElision::Conditional,
            (RuleNodeElision::Conditional, _) => RuleNodeElision::Conditional,
        }
    }
    fn concat(self, next: Self) -> Self {
        match (self, next) {
            (RuleNodeElision::None, next) => next,
            (RuleNodeElision::Unconditional, _) => RuleNodeElision::Unconditional,
            (RuleNodeElision::Conditional, RuleNodeElision::Unconditional) => {
                RuleNodeElision::Unconditional
            }
            (RuleNodeElision::Conditional, _) => RuleNodeElision::Conditional,
        }
    }
    fn opt(self) -> Self {
        match self {
            RuleNodeElision::None => RuleNodeElision::None,
            _ => RuleNodeElision::Conditional,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Recursion {
    Left(Regex, usize),
    Right(Regex, usize),
    LeftRight(Regex, usize, usize),
}
impl Recursion {
    pub fn regex(&self) -> Regex {
        match self {
            Recursion::Left(regex, ..) => *regex,
            Recursion::Right(regex, ..) => *regex,
            Recursion::LeftRight(regex, ..) => *regex,
        }
    }
}
#[derive(Default)]
pub struct RecursiveBranches {
    branches: Vec<Recursion>,
    binding_power: HashMap<NodeRef, (usize, usize)>,
    regex_map: HashMap<NodeRef, usize>,
}
impl RecursiveBranches {
    fn new(branches: Vec<Recursion>) -> Self {
        let mut binding_power = HashMap::new();
        let mut regex_map = HashMap::new();
        let max_bp = branches.len() * 2;
        for (i, branch) in branches.iter().enumerate() {
            regex_map.insert(branch.regex().syntax(), i);
            binding_power.insert(
                branch.regex().syntax(),
                (max_bp - i * 2, max_bp - i * 2 + 1),
            );
        }
        Self {
            branches,
            binding_power,
            regex_map,
        }
    }
    pub fn contains(&self, regex: Regex) -> bool {
        self.binding_power.contains_key(&regex.syntax())
    }
    pub fn get_branch(&self, regex: Regex) -> Option<Recursion> {
        self.regex_map
            .get(&regex.syntax())
            .map(|index| self.branches[*index])
    }
    pub fn branches(&self) -> &[Recursion] {
        &self.branches
    }
    pub fn binding_power(&self, regex: Regex) -> (usize, usize) {
        self.binding_power[&regex.syntax()]
    }
}

#[derive(Default)]
pub struct SemanticData<'a> {
    pub decl_bindings: HashMap<NodeRef, NodeRef>,
    pub recursive: BTreeMap<RuleDecl, RecursiveBranches>,
    pub elision: HashMap<NodeRef, RuleNodeElision>,
    pub right_associative: HashSet<&'a str>,
    pub skipped: BTreeSet<TokenDecl>,
    pub start: Option<RuleDecl>,
    pub predicates: BTreeMap<NodeRef, (&'a str, &'a str)>,
    pub actions: BTreeMap<NodeRef, (&'a str, &'a str)>,
    pub assertions: BTreeMap<NodeRef, (&'a str, &'a str)>,
    pub rule_bindings: BTreeMap<&'a str, Vec<NodeRef>>,
    pub first_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub follow_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub predict_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub recovery_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub left_rec_local_follow_sets: HashMap<NodeRef, BTreeSet<TokenName<'a>>>,
    pub used: HashSet<NodeRef>,
    pub has_rule_rename: HashSet<RuleDecl>,
    pub has_rule_creation: HashSet<RuleDecl>,
    pub undefined_rules: BTreeSet<&'a str>,
    pub undefined_tokens: BTreeSet<&'a str>,
    pub used_in_ordered_choice: HashSet<NodeRef>,
}

#[derive(Default)]
struct GeneralCheck<'a> {
    symbol_table: std::collections::HashMap<&'a str, NodeRef>,
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
            let span = cst.span(syntax);
            let old_span = cst.span(old);
            diags.push(Diagnostic::redefinition(&span, binding, &old_span));
        }
    }
    fn get_symbol_binding(
        &mut self,
        name: &'a str,
        rule_binding: bool,
        span: &Span,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) -> Option<NodeRef> {
        self.symbol_table
            .get(name)
            .or_else(|| {
                if rule_binding {
                    sema.undefined_rules.insert(name);
                    diags.push(Diagnostic::undefined_rule(span, name));
                } else {
                    sema.undefined_tokens.insert(name);
                    diags.push(Diagnostic::undefined_token(span, name));
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
        }
        if let Some(start) = sema.start {
            for (name, rule) in sema.decl_bindings.iter() {
                if *rule == start.syntax() {
                    diags.push(Diagnostic::reference_start_rule(&cst.span(*name)));
                }
            }
        } else {
            diags.push(Diagnostic::missing_start_rule());
        }
    }
    fn check_token_decl(&mut self, cst: &'a Cst, decl: TokenDecl, diags: &mut Vec<Diagnostic>) {
        if let Some((name, name_span)) = decl.name(cst) {
            if matches!(name, "EOF" | "Error") {
                diags.push(Diagnostic::predefined_name(&name_span, name));
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
            if matches!(name, "error") {
                diags.push(Diagnostic::predefined_name(&name_span, name));
            }
            self.bind_symbol(cst, name, "rule", decl.syntax(), diags);
            if name.starts_with(|c: char| c.is_uppercase()) {
                diags.push(Diagnostic::uppercase_rule(&name_span, name));
            }
        }
    }
    fn check_rule_decl(
        &mut self,
        cst: &'a Cst,
        rule: RuleDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        if let Some(regex) = rule.regex(cst) {
            self.check_regex(cst, rule, regex, diags, sema, false, false);
        } else {
            diags.push(Diagnostic::empty_rule(&rule.span(cst)));
        }
        self.check_recursive(cst, sema, rule, diags);
        if let Some(regex) = rule.regex(cst) {
            let mut open = HashSet::new();
            let mut created = HashMap::new();
            let mut used = HashSet::new();
            let left_rec = sema.recursive.get(&rule).is_some_and(|rec| {
                rec.branches()
                    .iter()
                    .any(|branch| matches!(branch, Recursion::Left(..) | Recursion::LeftRight(..)))
            });
            Self::check_node_creation(
                cst,
                regex,
                diags,
                &mut open,
                &mut created,
                &mut used,
                left_rec,
            );
            for (num, span) in created {
                if !used.contains(num) {
                    diags.push(Diagnostic::unused_node_marker(&span));
                }
            }
        }
    }
    fn check_start_decl(
        &mut self,
        cst: &'a Cst,
        start_decl: StartDecl,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
    ) {
        if let Some((name, name_span)) = start_decl.rule_name(cst) {
            if let Some(node) = self.get_symbol_binding(name, true, &name_span, diags, sema) {
                if let Some(rule_decl) = RuleDecl::cast(cst, node) {
                    sema.start = Some(rule_decl);
                } else {
                    diags.push(Diagnostic::expected_rule(&name_span));
                }
            }
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
            if let Some(node) = self.get_symbol_binding(name, false, &name_span, diags, sema) {
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
            if let Some(node) = self.get_symbol_binding(name, false, &name_span, diags, sema) {
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
        rule: RuleDecl,
        regex: Regex,
        diags: &mut Vec<Diagnostic>,
        sema: &mut SemanticData<'a>,
        in_alt: bool,
        in_loop: bool,
    ) -> RuleNodeElision {
        let elision = match regex {
            Regex::OrderedChoice(regex) => {
                regex
                    .operands(cst)
                    .fold(RuleNodeElision::None, |acc, regex| {
                        let elision = self.check_regex(cst, rule, regex, diags, sema, false, false);
                        acc.alt(elision)
                    })
            }
            Regex::Alternation(regex) => {
                regex
                    .operands(cst)
                    .fold(RuleNodeElision::None, |acc, regex| {
                        let elision = self.check_regex(cst, rule, regex, diags, sema, true, false);
                        acc.alt(elision)
                    })
            }
            Regex::Concat(regex) => {
                let mut in_alt = in_alt;
                let mut in_loop = in_loop;
                regex
                    .operands(cst)
                    .fold(RuleNodeElision::None, |acc, regex| {
                        let elision =
                            self.check_regex(cst, rule, regex, diags, sema, in_alt, in_loop);
                        in_alt = false;
                        in_loop = false;
                        if (acc == RuleNodeElision::Unconditional || rule.is_elided(cst))
                            && matches!(regex, Regex::NodeElision(_))
                        {
                            diags.push(Diagnostic::redundant_elision(&regex.span(cst)));
                        }
                        acc.concat(elision)
                    })
            }
            Regex::Paren(regex) => regex.inner(cst).map_or(RuleNodeElision::None, |regex| {
                self.check_regex(cst, rule, regex, diags, sema, false, in_loop)
            }),
            Regex::Optional(regex) => regex.operand(cst).map_or(RuleNodeElision::None, |regex| {
                self.check_regex(cst, rule, regex, diags, sema, false, true)
                    .opt()
            }),
            Regex::Star(regex) => regex
                .operand(cst)
                .map_or(RuleNodeElision::None, |regex| {
                    self.check_regex(cst, rule, regex, diags, sema, false, true)
                })
                .opt(),
            Regex::Plus(regex) => regex.operand(cst).map_or(RuleNodeElision::None, |regex| {
                self.check_regex(cst, rule, regex, diags, sema, false, true)
            }),
            Regex::Name(regex) => {
                if let Some((name, name_span)) = regex.value(cst) {
                    let rule_binding = name.starts_with(|c: char| c.is_lowercase());
                    if let Some(decl) =
                        self.get_symbol_binding(name, rule_binding, &name_span, diags, sema)
                    {
                        if let Some(token) = TokenDecl::cast(cst, decl) {
                            if sema.skipped.contains(&token) {
                                diags.push(Diagnostic::used_skipped(&name_span));
                            }
                        }
                        sema.decl_bindings.insert(regex.syntax(), decl);
                    }
                }
                RuleNodeElision::None
            }
            Regex::Symbol(regex) => {
                if let Some((name, name_span)) = regex.value(cst) {
                    if let Some(decl) =
                        self.get_symbol_binding(name, false, &name_span, diags, sema)
                    {
                        if let Some(token) = TokenDecl::cast(cst, decl) {
                            if sema.skipped.contains(&token) {
                                diags.push(Diagnostic::used_skipped(&name_span));
                            }
                        }
                        sema.decl_bindings.insert(regex.syntax(), decl);
                    }
                }
                RuleNodeElision::None
            }
            Regex::Predicate(regex) => {
                if let Some((value, value_span)) = regex.value(cst) {
                    if !in_alt && !in_loop {
                        diags.push(Diagnostic::invalid_predicate_pos(&value_span));
                    }
                    if !regex.is_true(cst) {
                        if let Some(rule_name) = rule.name(cst).map(|(name, _)| name) {
                            sema.predicates
                                .insert(regex.syntax(), (rule_name, &value[1..]));
                        }
                    }
                }
                RuleNodeElision::None
            }
            Regex::Action(regex) => {
                if let Some((value, _)) = regex.value(cst) {
                    if let Some(rule_name) = rule.name(cst).map(|(name, _)| name) {
                        sema.actions
                            .insert(regex.syntax(), (rule_name, &value[1..]));
                    }
                }
                RuleNodeElision::None
            }
            Regex::Assertion(regex) => {
                if let Some((value, _)) = regex.value(cst) {
                    if let Some(rule_name) = rule.name(cst).map(|(name, _)| name) {
                        sema.assertions
                            .insert(regex.syntax(), (rule_name, &value[1..]));
                    }
                }
                RuleNodeElision::None
            }
            Regex::NodeRename(regex) => {
                if let Some((name, name_span)) = regex.value(cst) {
                    let name = &name[1..];
                    if !name.is_empty() {
                        if name.starts_with(|c: char| c.is_uppercase()) {
                            diags.push(Diagnostic::uppercase_rule(&name_span, name));
                        }
                        sema.has_rule_rename.insert(rule);
                        sema.rule_bindings
                            .entry(name)
                            .and_modify(|val| val.push(regex.syntax()))
                            .or_insert(vec![regex.syntax()]);
                    } else {
                        diags.push(Diagnostic::missing_node_name(&name_span));
                    }
                }
                RuleNodeElision::None
            }
            Regex::NodeElision(_) => RuleNodeElision::Unconditional,
            Regex::NodeMarker(_) => RuleNodeElision::None,
            Regex::NodeCreation(regex) => {
                if let Some(name) = regex.node_name(cst) {
                    if !name.is_empty() {
                        if name.starts_with(|c: char| c.is_uppercase()) {
                            diags.push(Diagnostic::uppercase_rule(&regex.span(cst), name));
                        }
                        sema.rule_bindings
                            .entry(name)
                            .and_modify(|val| val.push(regex.syntax()))
                            .or_insert(vec![regex.syntax()]);
                    }
                }
                if regex.whole_rule(cst) {
                    sema.has_rule_creation.insert(rule);
                }
                RuleNodeElision::None
            }
            Regex::Commit(_) => RuleNodeElision::None,
        };
        sema.elision.insert(regex.syntax(), elision);
        elision
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
    fn check_recursive(
        &self,
        cst: &'a Cst,
        sema: &mut SemanticData,
        rule: RuleDecl,
        diags: &mut Vec<Diagnostic>,
    ) {
        let mut branches = vec![];
        if let Some(Regex::Alternation(alt)) = rule.regex(cst) {
            for alt_op in alt.operands(cst) {
                if let Regex::Concat(concat) = alt_op {
                    let mut concat_ops = concat.operands(cst).enumerate().filter(|(_, op)| {
                        !matches!(
                            op,
                            Regex::Predicate(_)
                                | Regex::NodeRename(_)
                                | Regex::NodeElision(_)
                                | Regex::Action(_)
                        )
                    });

                    let check_rec =
                        |(i, op)| self.name_references_rule(cst, sema, rule, op).then_some(i);
                    let left_rec = concat_ops.next().and_then(check_rec);
                    let right_rec = concat_ops.last().and_then(check_rec);

                    if left_rec.is_some()
                        && (sema
                            .elision
                            .get(&alt_op.syntax())
                            .is_some_and(|elision| *elision != RuleNodeElision::None)
                            || rule.is_elided(cst))
                    {
                        diags.push(Diagnostic::elide_left_rec(&alt_op.span(cst)));
                    }

                    match (left_rec, right_rec) {
                        (Some(left_rec), Some(right_rec)) => {
                            branches.push(Recursion::LeftRight(alt_op, left_rec, right_rec))
                        }
                        (Some(left_rec), None) => branches.push(Recursion::Left(alt_op, left_rec)),
                        (None, Some(right_rec)) => {
                            branches.push(Recursion::Right(alt_op, right_rec))
                        }
                        _ => {}
                    }
                }
            }
        }
        if !branches.is_empty() {
            sema.recursive
                .insert(rule, RecursiveBranches::new(branches));
        }
    }
    fn check_node_creation(
        cst: &'a Cst,
        regex: Regex,
        diags: &mut Vec<Diagnostic>,
        open: &mut HashSet<&'a str>,
        created: &mut HashMap<&'a str, Span>,
        used: &mut HashSet<&'a str>,
        left_rec: bool,
    ) {
        match regex {
            Regex::OrderedChoice(regex) => regex.operands(cst).for_each(|op| {
                Self::check_node_creation(
                    cst,
                    op,
                    diags,
                    &mut open.clone(),
                    created,
                    used,
                    left_rec,
                )
            }),
            Regex::Alternation(regex) => regex.operands(cst).for_each(|op| {
                Self::check_node_creation(
                    cst,
                    op,
                    diags,
                    &mut open.clone(),
                    created,
                    used,
                    left_rec,
                )
            }),
            Regex::Concat(regex) => {
                let old_open = open.clone();
                for op in regex.operands(cst) {
                    Self::check_node_creation(cst, op, diags, open, created, used, left_rec);
                }
                *open = old_open;
            }
            Regex::Paren(regex) => {
                if let Some(op) = regex.inner(cst) {
                    Self::check_node_creation(
                        cst,
                        op,
                        diags,
                        &mut open.clone(),
                        created,
                        used,
                        left_rec,
                    )
                }
            }
            Regex::Optional(regex) => {
                if let Some(op) = regex.operand(cst) {
                    Self::check_node_creation(
                        cst,
                        op,
                        diags,
                        &mut open.clone(),
                        created,
                        used,
                        left_rec,
                    )
                }
            }
            Regex::Star(regex) => {
                if let Some(op) = regex.operand(cst) {
                    Self::check_node_creation(
                        cst,
                        op,
                        diags,
                        &mut open.clone(),
                        created,
                        used,
                        left_rec,
                    )
                }
            }
            Regex::Plus(regex) => {
                if let Some(op) = regex.operand(cst) {
                    Self::check_node_creation(
                        cst,
                        op,
                        diags,
                        &mut open.clone(),
                        created,
                        used,
                        left_rec,
                    )
                }
            }
            Regex::NodeMarker(regex) => {
                let num = regex.number(cst);
                let span = regex.span(cst);
                open.insert(num);
                if let Some(old_span) = created.insert(regex.number(cst), span.clone()) {
                    diags.push(Diagnostic::redefine_node_marker(&span, &old_span));
                }
            }
            Regex::NodeCreation(regex) => {
                let span = regex.span(cst);
                if let Some(num) = regex.number(cst) {
                    used.insert(num);
                    if !open.contains(num) {
                        if let Some(open_span) = created.get(num) {
                            diags.push(Diagnostic::invalid_create_node(&span, open_span));
                        } else {
                            diags.push(Diagnostic::undefined_create_node(&span));
                        }
                    }
                } else if left_rec {
                    diags.push(Diagnostic::create_rule_node_left_rec(&span));
                }
            }
            Regex::Name(_)
            | Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::Commit(_) => {}
        };
    }
}

struct OrderedChoiceValidator;

impl<'a> OrderedChoiceValidator {
    fn run(cst: &'a Cst, diags: &mut Vec<Diagnostic>, sema: &mut SemanticData<'a>) {
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            Self::calc_containment(cst, sema, file);
            for rule in file.rule_decls(cst) {
                if let Some(regex) = rule.regex(cst) {
                    Self::check_containment(cst, sema, regex, diags);
                }
            }
        }
    }

    fn calc_containment(cst: &'a Cst, sema: &mut SemanticData<'a>, file: File) {
        loop {
            let size = sema.used_in_ordered_choice.len();
            for rule in file.rule_decls(cst) {
                if let Some(regex) = rule.regex(cst) {
                    Self::calc_containment_regex(
                        cst,
                        sema,
                        regex,
                        sema.used_in_ordered_choice.contains(&rule.syntax()),
                    );
                }
            }
            if sema.used_in_ordered_choice.len() == size {
                break;
            }
        }
    }

    fn calc_containment_regex(
        cst: &'a Cst,
        sema: &mut SemanticData<'a>,
        regex: Regex,
        active_choice: bool,
    ) {
        if active_choice {
            sema.used_in_ordered_choice.insert(regex.syntax());
        }
        match regex {
            Regex::Name(name) => {
                if active_choice {
                    let decl = sema.decl_bindings.get(&name.syntax());
                    if let Some(rule) = decl.and_then(|decl| RuleDecl::cast(cst, *decl)) {
                        sema.used_in_ordered_choice.insert(rule.syntax());
                    }
                }
            }
            Regex::Concat(concat) => {
                let mut active_choice = active_choice;
                for op in concat.operands(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                    if matches!(op, Regex::Commit(_)) {
                        active_choice = false;
                    }
                }
            }
            Regex::OrderedChoice(choice) => {
                let ops = choice.operands(cst).collect::<Vec<_>>();
                for op in &ops[..ops.len() - 1] {
                    Self::calc_containment_regex(cst, sema, *op, true);
                }
                let op = *ops.last().unwrap();
                Self::calc_containment_regex(cst, sema, op, active_choice);
            }
            Regex::Alternation(alt) => {
                for op in alt.operands(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                }
            }
            Regex::Paren(paren) => {
                if let Some(op) = paren.inner(cst) {
                    Self::calc_containment_regex(cst, sema, op, active_choice);
                }
            }
            Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {}
        };
    }
    fn check_containment(
        cst: &'a Cst,
        sema: &mut SemanticData<'a>,
        regex: Regex,
        diags: &mut Vec<Diagnostic>,
    ) {
        match regex {
            Regex::Concat(concat) => {
                for op in concat.operands(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::OrderedChoice(choice) => {
                if sema.used_in_ordered_choice.contains(&choice.syntax()) {
                    diags.push(Diagnostic::nested_ordered_choice(&choice.span(cst)));
                }
                for op in choice.operands(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Alternation(alt) => {
                for op in alt.operands(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Star(star) => {
                if let Some(op) = star.operand(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Optional(opt) => {
                if let Some(op) = opt.operand(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Plus(plus) => {
                if let Some(op) = plus.operand(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Paren(paren) => {
                if let Some(op) = paren.inner(cst) {
                    Self::check_containment(cst, sema, op, diags);
                }
            }
            Regex::Commit(commit) => {
                if !sema.used_in_ordered_choice.contains(&commit.syntax()) {
                    diags.push(Diagnostic::useless_commit(&commit.span(cst)));
                }
            }
            Regex::Action(action) => {
                if sema.used_in_ordered_choice.contains(&action.syntax()) {
                    diags.push(Diagnostic::action_in_ordered_choice(&action.span(cst)));
                }
            }
            Regex::Name(_)
            | Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_) => {}
        };
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
            Regex::OrderedChoice(choice) => {
                for op in choice.operands(cst) {
                    Self::calc_first_regex(cst, sema, op, change);
                    let op_first = sema.first_sets[&op.syntax()].clone();
                    sema.first_sets
                        .get_mut(&regex.syntax())
                        .unwrap()
                        .extend(op_first.into_iter());
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
            Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {
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
            Regex::OrderedChoice(choice) => {
                let follow = sema.follow_sets.entry(regex.syntax()).or_default().clone();
                for op in choice.operands(cst) {
                    sema.follow_sets
                        .entry(op.syntax())
                        .or_default()
                        .extend(follow.iter());
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
            Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {}
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
            if let Some(regex) = rule.regex(cst) {
                Self::check_regex(cst, sema, diags, regex, rule, sema.recursive.get(&rule));
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
                .is_some_and(|inner| Self::has_predicate(cst, inner)),
            _ => false,
        }
    }

    fn skip_first(cst: &Cst, op: Regex) -> Regex {
        let Regex::Concat(concat) = op else {
            unreachable!()
        };
        concat
            .operands(cst)
            .filter(|op| !matches!(op, Regex::Predicate(_)))
            .nth(1)
            .unwrap()
    }

    #[allow(clippy::too_many_arguments)]
    fn check_intersection(
        cst: &Cst,
        sema: &SemanticData<'a>,
        diags: &mut Vec<Diagnostic>,
        op: Regex,
        branches: impl Iterator<Item = Regex>,
        i: usize,
        left_rec: bool,
        ordered_choice: bool,
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
                related.push((cst.span(op.syntax()).clone(), set));
            }
        }
        if ordered_choice {
            if related.is_empty() {
                diags.push(Diagnostic::replaceable_ordered_choice(&op.span(cst)));
            }
        } else if !related.is_empty() {
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
        recursive: Option<&RecursiveBranches>,
    ) {
        match regex {
            Regex::Alternation(alt) => {
                let default_recursive = RecursiveBranches::default();
                let recursive = recursive.unwrap_or(&default_recursive);
                for (i, branch) in recursive
                    .branches
                    .iter()
                    .filter_map(|rec| match rec {
                        Recursion::Left(op, ..) | Recursion::LeftRight(op, ..) => Some(op),
                        _ => None,
                    })
                    .enumerate()
                {
                    let op = Self::skip_first(cst, *branch);

                    if !Self::has_predicate(cst, *branch) {
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
                                &cst.span(op.syntax()),
                                related,
                            ));
                        }

                        Self::check_intersection(
                            cst,
                            sema,
                            diags,
                            op,
                            recursive
                                .branches
                                .iter()
                                .filter_map(|rec| match rec {
                                    Recursion::Left(op, ..) | Recursion::LeftRight(op, ..) => {
                                        Some(op)
                                    }
                                    _ => None,
                                })
                                .copied(),
                            i,
                            true,
                            false,
                        );
                    }
                }
                let non_left_recursive_branches = || {
                    alt.operands(cst).filter(|op| {
                        matches!(recursive.get_branch(*op), None | Some(Recursion::Right(..)))
                    })
                };

                for (i, op) in non_left_recursive_branches().enumerate() {
                    if Self::has_predicate(cst, op) {
                        continue;
                    }
                    Self::check_intersection(
                        cst,
                        sema,
                        diags,
                        op,
                        non_left_recursive_branches(),
                        i,
                        false,
                        false,
                    );
                }
                for op in alt.operands(cst) {
                    Self::check_regex(cst, sema, diags, op, rule, None);
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
                    Self::check_regex(cst, sema, diags, op, rule, None);
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
                    Self::check_regex(cst, sema, diags, op, rule, None);
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
                    Self::check_regex(cst, sema, diags, op, rule, None);
                }
            }
            Regex::Name(name) => {
                if sema
                    .first_sets
                    .get(&name.syntax())
                    .is_some_and(|first| first.is_empty())
                {
                    diags.push(Diagnostic::consume_tokens(&regex.span(cst)));
                }
            }
            Regex::OrderedChoice(choice) => {
                let ops = choice.operands(cst).collect::<Vec<_>>();
                for (i, op) in ops.iter().enumerate() {
                    if i != ops.len() - 1 {
                        Self::check_intersection(
                            cst,
                            sema,
                            diags,
                            *op,
                            ops.iter().copied(),
                            i,
                            false,
                            true,
                        );
                    }
                    Self::check_regex(cst, sema, diags, *op, rule, None);
                }
            }
            Regex::Concat(concat) => {
                for op in concat.operands(cst) {
                    Self::check_regex(cst, sema, diags, op, rule, None);
                }
            }
            Regex::Paren(paren) => {
                if let Some(inner) = paren.inner(cst) {
                    Self::check_regex(cst, sema, diags, inner, rule, None);
                }
            }
            Regex::Symbol(_)
            | Regex::Predicate(_)
            | Regex::Action(_)
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {}
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
            Regex::OrderedChoice(choice) => choice
                .operands(cst)
                .for_each(|op| Self::set_regex(cst, sema, op)),
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
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {}
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
            Regex::OrderedChoice(choice) => {
                for op in choice.operands(cst) {
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
            | Regex::Assertion(_)
            | Regex::NodeRename(_)
            | Regex::NodeElision(_)
            | Regex::NodeMarker(_)
            | Regex::NodeCreation(_)
            | Regex::Commit(_) => {}
        }
    }
}

struct OperatorValidator;

impl OperatorValidator {
    fn run(cst: &Cst, diags: &mut Vec<Diagnostic>, sema: &mut SemanticData) {
        for recursive in sema.recursive.values_mut() {
            for branch in recursive.branches.iter() {
                if let Recursion::LeftRight(regex @ Regex::Concat(concat), left_index, ..) = branch
                {
                    let mut ops = concat.operands(cst);
                    let operand = ops.nth(left_index + 1).unwrap();
                    let mut left_assoc = false;
                    let mut right_assoc = false;
                    for sym in sema.first_sets[&operand.syntax()].iter() {
                        if sema.right_associative.contains(sym.0) {
                            right_assoc = true;
                            recursive
                                .binding_power
                                .entry(regex.syntax())
                                .and_modify(|e| *e = (e.1, e.0));
                        } else {
                            left_assoc = true;
                        }
                    }
                    if right_assoc && left_assoc {
                        diags.push(Diagnostic::mixed_assoc(&operand.span(cst)));
                    }
                }
            }
        }
    }
}
