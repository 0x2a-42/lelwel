use codespan_reporting::diagnostic::Label;

use super::parser::{Diagnostic, Span};

pub const INVALID_BINDING_POS: &str = "E001";
pub const INVALID_PREDICATE_POS: &str = "E002";
pub const UNDEFINED_RULE: &str = "E003";
pub const UNDEFINED_TOKEN: &str = "E004";
pub const REDEFINITION: &str = "E005";
pub const UPPERCASE_RULE: &str = "E006";
pub const LOWERCASE_TOKEN: &str = "E007";
pub const MISSING_START_RULE: &str = "E008";
pub const REFERENCE_START_RULE: &str = "E009";
pub const PREDEFINED_NAME: &str = "E010";
pub const LL1_CONFLICT_ALT: &str = "E011";
pub const LL1_CONFLICT_LEFT_REC: &str = "E012";
pub const LL1_CONFLICT_REP: &str = "E013";
pub const LL1_CONFLICT_OPT: &str = "E014";
pub const CONSUME_TOKENS: &str = "E015";
pub const REDEFINE_AS_SKIPPED: &str = "E016";
pub const USED_SKIPPED: &str = "E017";
pub const EXPECTED_TOKEN: &str = "E018";
pub const REDEFINE_AS_RIGHT: &str = "E019";
pub const MIXED_ASSOC: &str = "E020";
pub const ELIDE_LEFT_REC: &str = "E021";
pub const REDEF_OPEN_NODE: &str = "E022";
pub const UNDEF_CLOSE_NODE: &str = "E023";
pub const INVALID_CLOSE_NODE: &str = "E024";
pub const CREATE_RULE_NODE_LEFT_REC: &str = "E025";
pub const EXPECTED_RULE: &str = "E026";
pub const MISSING_NODE_NAME: &str = "E027";
pub const NESTED_ORDERED_CHOICE: &str = "E028";
pub const ACTION_IN_ORDERED_CHOICE: &str = "E029";

pub const UNUSED_RULE: &str = "W001";
pub const UNUSED_TOKEN: &str = "W002";
pub const UNUSED_OPEN_NODE: &str = "W003";
pub const REDUNDANT_ELISION: &str = "W004";
pub const EMPTY_RULE: &str = "W005";
pub const USELESS_COMMIT: &str = "W006";
pub const REPLACEABLE_ORDERED_CHOICE: &str = "W007";

pub trait LanguageErrors {
    fn invalid_binding_pos(span: &Span) -> Self;
    fn invalid_predicate_pos(span: &Span) -> Self;
    fn undefined_rule(span: &Span, name: &str) -> Self;
    fn undefined_token(span: &Span, name: &str) -> Self;
    fn redefinition(span: &Span, binding: &str, old_span: &Span) -> Self;
    fn uppercase_rule(span: &Span, name: &str) -> Self;
    fn lowercase_token(span: &Span, name: &str) -> Self;
    fn missing_start_rule() -> Self;
    fn reference_start_rule(span: &Span) -> Self;
    fn predefined_name(span: &Span, name: &str) -> Self;
    fn unused_rule(span: &Span) -> Self;
    fn unused_token(span: &Span) -> Self;
    fn ll1_conflict_alt(span: &Span, conflicting: Vec<(Span, String)>) -> Self;
    fn ll1_conflict_left_rec(span: &Span, conflicting: Vec<(Span, String)>) -> Self;
    fn ll1_conflict_rep(span: &Span, conflicting: String) -> Self;
    fn ll1_conflict_opt(span: &Span, conflicting: String) -> Self;
    fn consume_tokens(span: &Span) -> Self;
    fn redefine_as_skipped(span: &Span) -> Self;
    fn used_skipped(span: &Span) -> Self;
    fn expected_token(span: &Span) -> Self;
    fn redefine_as_right(span: &Span) -> Self;
    fn mixed_assoc(span: &Span) -> Self;
    fn elide_left_rec(span: &Span) -> Self;
    fn redefine_node_marker(span: &Span, old_span: &Span) -> Self;
    fn undefined_create_node(span: &Span) -> Self;
    fn invalid_create_node(span: &Span, open_span: &Span) -> Self;
    fn create_rule_node_left_rec(span: &Span) -> Self;
    fn unused_node_marker(span: &Span) -> Self;
    fn redundant_elision(span: &Span) -> Self;
    fn expected_rule(span: &Span) -> Self;
    fn missing_node_name(span: &Span) -> Self;
    fn empty_rule(span: &Span) -> Self;
    fn nested_ordered_choice(span: &Span) -> Self;
    fn useless_commit(span: &Span) -> Self;
    fn replaceable_ordered_choice(span: &Span) -> Self;
    fn action_in_ordered_choice(span: &Span) -> Self;
}

impl LanguageErrors for Diagnostic {
    fn invalid_binding_pos(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(INVALID_BINDING_POS)
            .with_message("invalid binding position")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: bindings may appear at the end of a concatenation".to_string(),
            ])
    }

    fn invalid_predicate_pos(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(INVALID_PREDICATE_POS)
            .with_message("invalid predicate position")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: predicates may appear at the start of an alternation branch".to_string(),
                "note: predicates may appear at the start of a `*`, `+`, or `[]` regex".to_string(),
            ])
    }

    fn undefined_rule(span: &Span, name: &str) -> Self {
        Diagnostic::error()
            .with_code(UNDEFINED_RULE)
            .with_message(format!("use of undefined rule `{name}`"))
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn undefined_token(span: &Span, name: &str) -> Self {
        Diagnostic::error()
            .with_code(UNDEFINED_TOKEN)
            .with_message(format!("use of undefined token `{name}`"))
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redefinition(span: &Span, binding: &str, old_span: &Span) -> Self {
        Diagnostic::error()
            .with_code(REDEFINITION)
            .with_message(format!("redefinition of {}", binding,))
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), old_span.clone()).with_message("previous definition"),
            ])
    }

    fn uppercase_rule(span: &Span, name: &str) -> Self {
        Diagnostic::error()
            .with_code(UPPERCASE_RULE)
            .with_message("rule name starts with upper case letter")
            .with_labels(vec![Label::primary((), span.clone()).with_message(
                format!(
                    "rename to `{}{}`",
                    name.chars().next().unwrap().to_lowercase(),
                    name.chars().skip(1).collect::<String>()
                ),
            )])
            .with_notes(vec![
                "note: rule names must start with a lower case letter".to_string()
            ])
    }

    fn lowercase_token(span: &Span, name: &str) -> Self {
        Diagnostic::error()
            .with_code(LOWERCASE_TOKEN)
            .with_message("token name starts with lower case letter")
            .with_labels(vec![Label::primary((), span.clone()).with_message(
                format!(
                    "rename to `{}{}`",
                    name.chars().next().unwrap().to_uppercase(),
                    name.chars().skip(1).collect::<String>()
                ),
            )])
            .with_notes(vec![
                "note: token names must start with an upper case letter".to_string(),
            ])
    }

    fn missing_start_rule() -> Self {
        Diagnostic::error()
            .with_code(MISSING_START_RULE)
            .with_message("missing start rule")
            .with_notes(vec![
                "help: specify the start rule with\n\nstart rule_name;".to_string(),
            ])
    }

    fn reference_start_rule(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(REFERENCE_START_RULE)
            .with_message("cannot reference start rule in regex")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn predefined_name(span: &Span, name: &str) -> Self {
        let kind = if name.starts_with(|c: char| c.is_uppercase()) {
            "token"
        } else {
            "rule"
        };
        let mut notes = vec![format!("note: the {kind} name `{name}` is reserved")];
        if name == "EOF" {
            notes.push("note: there is no need for an explicit EOF token".to_string())
        }
        Diagnostic::error()
            .with_code(PREDEFINED_NAME)
            .with_message(format!("use of predefined {kind} name"))
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(notes)
    }

    fn ll1_conflict_alt(span: &Span, conflicting: Vec<(Span, String)>) -> Self {
        let mut labels = vec![Label::primary((), span.clone())];
        labels.extend(
            conflicting
                .into_iter()
                .map(|(span, msg)| Label::secondary((), span).with_message(msg)),
        );
        Diagnostic::error()
            .with_code(LL1_CONFLICT_ALT)
            .with_message("LL(1) conflict in alternation")
            .with_labels(labels)
            .with_notes(vec![
                "note: transform the grammar or add a predicate to the first branch".to_string(),
            ])
    }

    fn ll1_conflict_left_rec(span: &Span, conflicting: Vec<(Span, String)>) -> Self {
        let mut labels = vec![Label::primary((), span.clone())];
        labels.extend(
            conflicting
                .into_iter()
                .map(|(span, msg)| Label::secondary((), span).with_message(msg)),
        );
        Diagnostic::error()
            .with_code(LL1_CONFLICT_LEFT_REC)
            .with_message("LL(1) conflict in left recursive rule")
            .with_labels(labels)
    }

    fn ll1_conflict_rep(span: &Span, conflicting: String) -> Self {
        Diagnostic::error()
            .with_code(LL1_CONFLICT_REP)
            .with_message("LL(1) conflict in repetition")
            .with_labels(vec![
                Label::primary((), span.clone()).with_message(conflicting)
            ])
            .with_notes(vec![
                "note: transform the grammar or add a predicate to the start of the repetition"
                    .to_string(),
            ])
    }

    fn ll1_conflict_opt(span: &Span, conflicting: String) -> Self {
        Diagnostic::error()
            .with_code(LL1_CONFLICT_OPT)
            .with_message("LL(1) conflict in option")
            .with_labels(vec![
                Label::primary((), span.clone()).with_message(conflicting)
            ])
            .with_notes(vec![
                "note: transform the grammar or add a predicate to the start of the option"
                    .to_string(),
            ])
    }

    fn consume_tokens(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(CONSUME_TOKENS)
            .with_message("no tokens consumed")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: such a rule would cause a stack overflow".to_string()
            ])
    }

    fn redefine_as_skipped(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(REDEFINE_AS_SKIPPED)
            .with_message("token is already skipped")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn used_skipped(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(USED_SKIPPED)
            .with_message("skipped token cannot be used in regex")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn expected_token(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(EXPECTED_TOKEN)
            .with_message("expected token")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redefine_as_right(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(REDEFINE_AS_RIGHT)
            .with_message("token is already right associative")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn unused_rule(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(UNUSED_RULE)
            .with_message("unused rule")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn unused_token(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(UNUSED_TOKEN)
            .with_message("unused token")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn mixed_assoc(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(MIXED_ASSOC)
            .with_message("mixed associativity in infix operator branch")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn elide_left_rec(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(ELIDE_LEFT_REC)
            .with_message("left recursive rule branch cannot be elided")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redefine_node_marker(span: &Span, old_span: &Span) -> Self {
        Diagnostic::error()
            .with_code(REDEF_OPEN_NODE)
            .with_message("node marker was already defined")
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), old_span.clone()).with_message("previous definition"),
            ])
            .with_notes(vec![
                "note: the index of a node marker must be unique inside a rule".to_string(),
            ])
    }

    fn undefined_create_node(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(UNDEF_CLOSE_NODE)
            .with_message("node creation with undefined node marker")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn invalid_create_node(span: &Span, open_span: &Span) -> Self {
        Diagnostic::error()
            .with_code(INVALID_CLOSE_NODE)
            .with_message("node marker is not visited before node creation")
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), open_span.clone()).with_message("node marked here"),
            ])
    }

    fn create_rule_node_left_rec(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(CREATE_RULE_NODE_LEFT_REC)
            .with_message("node creation without index is not allowed in left recursive rules")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn unused_node_marker(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(UNUSED_OPEN_NODE)
            .with_message("unused node marker")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redundant_elision(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(REDUNDANT_ELISION)
            .with_message("node elision is redundant")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn expected_rule(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(EXPECTED_RULE)
            .with_message("expected rule")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn missing_node_name(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(MISSING_NODE_NAME)
            .with_message("missing node name")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn empty_rule(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(EMPTY_RULE)
            .with_message("empty rule")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn nested_ordered_choice(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(NESTED_ORDERED_CHOICE)
            .with_message("nested ordered choice")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn useless_commit(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(USELESS_COMMIT)
            .with_message("commit is never used in ordered choice")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn replaceable_ordered_choice(span: &Span) -> Self {
        Diagnostic::warning()
            .with_code(REPLACEABLE_ORDERED_CHOICE)
            .with_message("ordered choice branch could be moved to alternation")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn action_in_ordered_choice(span: &Span) -> Self {
        Diagnostic::error()
            .with_code(ACTION_IN_ORDERED_CHOICE)
            .with_message("semantic action could be used inside of ordered choice")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: this is not allowed to prevent side effects where backtracking is possible"
                    .to_string(),
            ])
    }
}
