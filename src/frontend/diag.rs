use codespan_reporting::diagnostic::Label;

use super::parser::{Diagnostic, Span};

pub trait LanguageErrors<'a> {
    fn invalid_binding_pos(span: &Span) -> Self;
    fn invalid_predicate_pos(span: &Span) -> Self;
    fn undefined_rule(span: &Span, name: &str, used_span: &Span) -> Self;
    fn missing_definition(span: &Span, def_kind: &str, name: &str) -> Self;
    fn redefinition(span: &Span, binding: &str, old_span: &Span) -> Self;
    fn uppercase_rule(span: &Span, name: &str) -> Self;
    fn lowercase_token(span: &Span, name: &str) -> Self;
    fn missing_start_rule() -> Self;
    fn reference_start_rule(span: &Span) -> Self;
    fn predefined_token_name(span: &Span) -> Self;
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
}

impl<'a> LanguageErrors<'a> for Diagnostic {
    fn invalid_binding_pos(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E001")
            .with_message("invalid binding position")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: bindings may appear at the end of a concatenation".to_string(),
            ])
    }

    fn invalid_predicate_pos(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E002")
            .with_message("invalid predicate position")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: predicates may appear at the start of an alternation branch".to_string(),
                "note: predicates may appear at the start of a `*`, `+`, or `[]` regex".to_string(),
            ])
    }

    fn undefined_rule(span: &Span, name: &str, used_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E003")
            .with_message(format!("use of undefined rule `{name}`"))
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), used_span.clone()).with_message("in this definition"),
            ])
    }

    fn missing_definition(span: &Span, def_kind: &str, name: &str) -> Self {
        Diagnostic::error()
            .with_code("E004")
            .with_message(format!("use of undefined {def_kind} `{name}`"))
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redefinition(span: &Span, binding: &str, old_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E005")
            .with_message(format!("redefinition of {}", binding,))
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), old_span.clone()).with_message("previous definition"),
            ])
    }

    fn uppercase_rule(span: &Span, name: &str) -> Self {
        Diagnostic::error()
            .with_code("E006")
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
            .with_code("E007")
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
            .with_code("E008")
            .with_message("missing start rule")
            .with_notes(vec![
                "help: specify the start rule with\n\nstart rule_name;".to_string(),
            ])
    }

    fn reference_start_rule(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E009")
            .with_message("cannot reference start rule in regex")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn predefined_token_name(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E010")
            .with_message("use of predefined token name")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: the token name `EOF` is reserved".to_string(),
                "note: there is no need for an explicit EOF token".to_string(),
            ])
    }

    fn unused_rule(span: &Span) -> Self {
        Diagnostic::warning()
            .with_message("unused rule")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn unused_token(span: &Span) -> Self {
        Diagnostic::warning()
            .with_message("unused token")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn ll1_conflict_alt(span: &Span, conflicting: Vec<(Span, String)>) -> Self {
        let mut labels = vec![Label::primary((), span.clone())];
        labels.extend(
            conflicting
                .into_iter()
                .map(|(span, msg)| Label::secondary((), span).with_message(msg)),
        );
        Diagnostic::error()
            .with_code("E011")
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
            .with_code("E012")
            .with_message("LL(1) conflict in left recursive rule")
            .with_labels(labels)
    }

    fn ll1_conflict_rep(span: &Span, conflicting: String) -> Self {
        Diagnostic::error()
            .with_code("E013")
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
            .with_code("E014")
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
            .with_code("E015")
            .with_message("no tokens consumed")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: such a rule would cause a stack overflow".to_string()
            ])
    }

    fn redefine_as_skipped(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E016")
            .with_message("token is already skipped")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn used_skipped(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E017")
            .with_message("skipped token cannot be used in regex")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn expected_token(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E018")
            .with_message("expected token")
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn redefine_as_right(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E019")
            .with_message("token is already right associative")
            .with_labels(vec![Label::primary((), span.clone())])
    }
}
