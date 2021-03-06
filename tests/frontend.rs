// This file was generated by the generate.sh script.
// DO NOT EDIT THIS FILE MANUALLY!

use lelwel::frontend::{ast::*, diag::*};

macro_rules! check_next {
    ($diags: ident, $message: expr) => {
        assert_eq!($diags.next().unwrap().to_string(), $message);
    };
}
macro_rules! check_empty {
    ($diags: ident) => {
        if !$diags.next().is_none() {
            panic!("too many {}", stringify!($diags))
        }
    };
}

fn gen_diag(input: &str) -> std::io::Result<Diag> {
    let ast = Ast::new();
    let contents = std::fs::read_to_string(input)?;
    let diag = lelwel::run_frontend(input, contents, &ast);
    Ok(diag)
}

#[test]
#[rustfmt::skip]
fn calc() {
    let diag = gen_diag("tests/frontend/calc.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn empty() {
    let diag = gen_diag("tests/frontend/empty.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/empty.llw:1:1-1:1: missing start rule");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn error_syntax() {
    let diag = gen_diag("tests/frontend/error_syntax.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/error_syntax.llw:6:3-6:5: error handler can only occur in one alternation branch");
    check_next!(errors, "tests/frontend/error_syntax.llw:19:3-19:9: error handler must be only term in alternation branch or last term of concatenation");
    check_next!(errors, "tests/frontend/error_syntax.llw:24:3-24:7: error handler must be only term in alternation branch or last term of concatenation");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn expected_action() {
    let diag = gen_diag("tests/frontend/expected_action.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/expected_action.llw:2:21-2:23: expected action with number 3 or 4");
    check_next!(errors, "tests/frontend/expected_action.llw:2:24-2:26: expected action with number 3 or 4");
    check_next!(errors, "tests/frontend/expected_action.llw:2:27-2:29: expected action with number 3 or 4");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn expected_error_handler() {
    let diag = gen_diag("tests/frontend/expected_error_handler.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/expected_error_handler.llw:4:10-4:12: expected error handler with number 1");
    check_next!(errors, "tests/frontend/expected_error_handler.llw:4:26-4:28: expected error handler with number 2 or 3");
    check_next!(errors, "tests/frontend/expected_error_handler.llw:4:30-4:32: expected error handler with number 2 or 3");
    check_empty!(errors);

    check_next!(warnings, "tests/frontend/expected_error_handler.llw:4:10-4:12: undefined error handler");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn expected_predicate() {
    let diag = gen_diag("tests/frontend/expected_predicate.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/expected_predicate.llw:4:3-4:5: expected predicate with number 1");
    check_next!(errors, "tests/frontend/expected_predicate.llw:9:3-9:5: expected predicate with number 3 or 4");
    check_next!(errors, "tests/frontend/expected_predicate.llw:10:3-10:5: expected predicate with number 3 or 4");
    check_empty!(errors);

    check_next!(warnings, "tests/frontend/expected_predicate.llw:4:3-4:5: undefined predicate");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn invalid_lang() {
    let diag = gen_diag("tests/frontend/invalid_lang.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/invalid_lang.llw:1:10-1:17: invalid target language 'unknown'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn invalid_token() {
    let diag = gen_diag("tests/frontend/invalid_token.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/invalid_token.llw:4:5-4:6: invalid token");
    check_next!(errors, "tests/frontend/invalid_token.llw:4:7-4:8: invalid token");
    check_next!(errors, "tests/frontend/invalid_token.llw:4:8-4:9: invalid token");
    check_next!(errors, "tests/frontend/invalid_token.llw:4:9-4:10: invalid token");
    check_next!(errors, "tests/frontend/invalid_token.llw:4:11-4:12: invalid token");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn json() {
    let diag = gen_diag("tests/frontend/json.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn left_recursive() {
    let diag = gen_diag("tests/frontend/left_recursive.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/left_recursive.llw:2:3-2:4: no tokens consumed");
    check_next!(errors, "tests/frontend/left_recursive.llw:6:3-6:4: no tokens consumed");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn lelwel() {
    let diag = gen_diag("tests/frontend/lelwel.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn ll1_conflict() {
    let diag = gen_diag("tests/frontend/ll1_conflict.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/ll1_conflict.llw:4:4-4:5: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:5:4-5:5: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:6:3-6:6: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:7:3-7:6: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:8:4-8:5: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:19:4-19:5: LL(1) confilict");
    check_next!(errors, "tests/frontend/ll1_conflict.llw:30:3-30:4: LL(1) confilict");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn lowercase_token() {
    let diag = gen_diag("tests/frontend/lowercase_token.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/lowercase_token.llw:1:7-1:8: token 'a' must start with uppercase letter or '_'");
    check_next!(errors, "tests/frontend/lowercase_token.llw:1:9-1:11: token '_b' must start with uppercase letter or '_'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn predef_token() {
    let diag = gen_diag("tests/frontend/predef_token.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/predef_token.llw:1:7-1:10: cannot use predefined token name");
    check_next!(errors, "tests/frontend/predef_token.llw:4:11-4:14: undefined element 'EOF'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn predicate_position() {
    let diag = gen_diag("tests/frontend/predicate_position.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/predicate_position.llw:4:3-4:5: predicate must be first term in alternation branch");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn recursion_depth() {
    let diag = gen_diag("tests/frontend/recursion_depth.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/recursion_depth.llw:2:35-2:35: exceeded recursion depth limit");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn redefinition() {
    let diag = gen_diag("tests/frontend/redefinition.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/redefinition.llw:1:9-1:10: redefinition of element 'A'");
    check_next!(errors, "tests/frontend/redefinition.llw:1:13-1:14: redefinition of element 'B'");
    check_next!(errors, "tests/frontend/redefinition.llw:9:1-10:2: redefinition of element 'a'");
    check_next!(errors, "tests/frontend/redefinition.llw:14:1-15:2: redefinition of element 'b'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn syntax_error() {
    let diag = gen_diag("tests/frontend/syntax_error.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/syntax_error.llw:2:1-2:6: invalid syntax, expected one of: ';', '=', <identifier>, <code segment>.");
    check_next!(errors, "tests/frontend/syntax_error.llw:6:1-6:2: invalid syntax, expected: ')'.");
    check_next!(errors, "tests/frontend/syntax_error.llw:11:2-11:3: invalid syntax, expected one of: ';', '(', ')', '[', ']', '|', '*', '+', <identifier>, <string literal>, <semantic predicate>, <semantic action>, <error handler>.");
    check_next!(errors, "tests/frontend/syntax_error.llw:11:1-11:2: undefined element 'b'");
    check_next!(errors, "tests/frontend/syntax_error.llw:16:3-16:4: undefined element 'C'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn undefined_action() {
    let diag = gen_diag("tests/frontend/undefined_action.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_next!(warnings, "tests/frontend/undefined_action.llw:2:3-2:5: undefined action");
    check_next!(warnings, "tests/frontend/undefined_action.llw:2:11-2:13: undefined action");
    check_next!(warnings, "tests/frontend/undefined_action.llw:6:3-6:5: undefined action");
    check_next!(warnings, "tests/frontend/undefined_action.llw:6:6-6:8: undefined action");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn undefined_error_handler() {
    let diag = gen_diag("tests/frontend/undefined_error_handler.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_next!(warnings, "tests/frontend/undefined_error_handler.llw:2:11-2:13: undefined error handler");
    check_next!(warnings, "tests/frontend/undefined_error_handler.llw:2:15-2:17: undefined error handler");
    check_next!(warnings, "tests/frontend/undefined_error_handler.llw:6:5-6:7: undefined error handler");
    check_next!(warnings, "tests/frontend/undefined_error_handler.llw:6:13-6:15: undefined error handler");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn undefined() {
    let diag = gen_diag("tests/frontend/undefined.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/undefined.llw:2:3-2:4: undefined element 'A'");
    check_next!(errors, "tests/frontend/undefined.llw:2:5-2:6: undefined element 'b'");
    check_next!(errors, "tests/frontend/undefined.llw:2:7-2:8: undefined element 'C'");
    check_next!(errors, "tests/frontend/undefined.llw:2:9-2:10: undefined element 'd'");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn undefined_predicate() {
    let diag = gen_diag("tests/frontend/undefined_predicate.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_next!(warnings, "tests/frontend/undefined_predicate.llw:5:3-5:5: undefined predicate");
    check_next!(warnings, "tests/frontend/undefined_predicate.llw:6:3-6:5: undefined predicate");
    check_next!(warnings, "tests/frontend/undefined_predicate.llw:10:3-10:5: undefined predicate");
    check_next!(warnings, "tests/frontend/undefined_predicate.llw:12:3-12:5: undefined predicate");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn unused_element() {
    let diag = gen_diag("tests/frontend/unused_element.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_next!(warnings, "tests/frontend/unused_element.llw:1:7-1:8: unused element");
    check_next!(warnings, "tests/frontend/unused_element.llw:7:1-9:2: unused element");
    check_next!(warnings, "tests/frontend/unused_element.llw:11:1-12:2: unused element");
    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn uppercase_rule() {
    let diag = gen_diag("tests/frontend/uppercase_rule.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_next!(errors, "tests/frontend/uppercase_rule.llw:4:1-5:2: rule 'A' must start with lowercase letter");
    check_next!(errors, "tests/frontend/uppercase_rule.llw:7:1-8:2: rule '_B' must start with lowercase letter");
    check_empty!(errors);

    check_empty!(warnings);
}

#[test]
#[rustfmt::skip]
fn valid_lang() {
    let diag = gen_diag("tests/frontend/valid_lang.llw").unwrap();
    let mut errors = diag.error_iter();
    let mut warnings = diag.warning_iter();

    check_empty!(errors);

    check_empty!(warnings);
}
