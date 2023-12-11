use codespan_reporting::diagnostic::Label;

use super::parser::{Diagnostic, Span};
use super::sema::Binding;
use super::symbols::Symbol;

pub trait LanguageErrors<'a> {
    fn invalid_error_handler_pos(span: &Span, correct_span: &Span) -> Self;
    fn invalid_predicate_pos(span: &Span) -> Self;
    fn multiple_error_handler_alt(span: &Span, other_span: &Span, alt_span: &Span) -> Self;
    fn undefined_rule(span: &Span, name: Symbol<'a>, used_span: &Span) -> Self;
    fn missing_definition(span: &Span, def_kind: &str, name: Symbol<'a>) -> Self;
    fn missing_definition_warning(
        span: &Span,
        def_kind: &str,
        rule_name: Symbol<'a>,
        prefix: &str,
        num: u64,
    ) -> Self;
    fn redefinition(span: &Span, binding: Binding, old_span: &Span) -> Self;
    fn invalid_number(span: &Span, def_kind: &str, prefix: &str, num: u64) -> Self;
    fn uppercase_rule(span: &Span, name: Symbol<'a>) -> Self;
    fn lowercase_token(span: &Span, name: Symbol<'a>) -> Self;
    fn missing_start_rule() -> Self;
    fn predefined_token_name(span: &Span) -> Self;
    fn unused_element(span: &Span) -> Self;
    fn ll1_conflict_alt(span: &Span, conflicting: Vec<(Span, String)>) -> Self;
    fn ll1_conflict_rep(span: &Span, conflicting: String) -> Self;
    fn ll1_conflict_opt(span: &Span, conflicting: String) -> Self;
    fn consume_tokens(span: &Span) -> Self;
}

impl<'a> LanguageErrors<'a> for Diagnostic {
    fn invalid_error_handler_pos(span: &Span, correct_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E001")
            .with_message("invalid error handler position")
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), correct_span.clone()).with_message("move it after this item"),
            ])
            .with_notes(vec![
                "note: error handlers may appear at the end of a concatenation".to_string(),
                "note: error handlers may appear as a single term in an alternation branch"
                    .to_string(),
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

    fn multiple_error_handler_alt(span: &Span, other_span: &Span, alt_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E003")
            .with_message("multiple error handlers in alternation")
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), other_span.clone()).with_message("other error handler branch"),
                Label::secondary((), alt_span.clone()).with_message("in this alternation"),
            ])
            .with_notes(vec![
                "note: alternations must contain at most one error handler branch".to_string(),
            ])
    }

    fn undefined_rule(span: &Span, name: Symbol<'a>, used_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E004")
            .with_message(format!("use of undefined rule `{name}`"))
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), used_span.clone()).with_message("in this definition"),
            ])
    }

    fn missing_definition(span: &Span, def_kind: &str, name: Symbol<'a>) -> Self {
        Diagnostic::error()
            .with_code("E005")
            .with_message(format!("use of undefined {def_kind} `{name}`"))
            .with_labels(vec![Label::primary((), span.clone())])
    }

    fn missing_definition_warning(
        span: &Span,
        def_kind: &str,
        rule_name: Symbol<'a>,
        prefix: &str,
        num: u64,
    ) -> Self {
        Diagnostic::warning()
            .with_message(format!("use of undefined {}", def_kind))
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![format!(
                "help: define the {def_kind} with\n\n{rule_name}{prefix}{num} {{\n    /* ... */\n}}",
            )])
    }

    fn redefinition(span: &Span, binding: Binding, old_span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E006")
            .with_message(format!("redefinition of {}", binding,))
            .with_labels(vec![
                Label::primary((), span.clone()),
                Label::secondary((), old_span.clone()).with_message("previous definition"),
            ])
    }

    fn invalid_number(span: &Span, def_kind: &str, prefix: &str, num: u64) -> Self {
        Diagnostic::error()
            .with_code("E007")
            .with_message(format!("invalid {def_kind} number"))
            .with_labels(vec![Label::primary((), span.clone()).with_message(
                format!("expected `{prefix}{num}` or `{prefix}{}`", num - 1),
            )])
            .with_notes(vec![
                "note: number must be greater or equal to the preceding number".to_string(),
            ])
    }

    fn uppercase_rule(span: &Span, name: Symbol<'a>) -> Self {
        Diagnostic::error()
            .with_code("E008")
            .with_message("rule name starts with upper case letter")
            .with_labels(vec![Label::primary((), span.clone()).with_message(
                format!(
                    "rename to `{}{}`",
                    name.0.chars().next().unwrap().to_lowercase(),
                    name.0.chars().skip(1).collect::<String>()
                ),
            )])
            .with_notes(vec![
                "note: rule names must start with a lower case letter".to_string()
            ])
    }

    fn lowercase_token(span: &Span, name: Symbol<'a>) -> Self {
        Diagnostic::error()
            .with_code("E009")
            .with_message("token name starts with lower case letter")
            .with_labels(vec![Label::primary((), span.clone()).with_message(
                format!(
                    "rename to `{}{}`",
                    name.0.chars().next().unwrap().to_uppercase(),
                    name.0.chars().skip(1).collect::<String>()
                ),
            )])
            .with_notes(vec![
                "note: token names must start with an upper case letter".to_string(),
            ])
    }

    fn missing_start_rule() -> Self {
        Diagnostic::error()
            .with_code("E010")
            .with_message("missing start rule")
            .with_notes(vec![
                "help: define the rule with\n\nstart {\n    /* ... */\n}".to_string(),
            ])
    }

    fn predefined_token_name(span: &Span) -> Self {
        Diagnostic::error()
            .with_code("E011")
            .with_message("use of predefined token name")
            .with_labels(vec![Label::primary((), span.clone())])
            .with_notes(vec![
                "note: the token name `EOF` is reserved".to_string(),
                "note: there is no need for an explicit EOF token".to_string(),
            ])
    }

    fn unused_element(span: &Span) -> Self {
        Diagnostic::warning()
            .with_message("unused element")
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
            .with_code("E012")
            .with_message("LL(1) conflict in alternation")
            .with_labels(labels)
            .with_notes(vec![
                "note: transform the grammar or add a predicate to the first branch".to_string(),
            ])
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
}
