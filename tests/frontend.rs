// This file was generated by the generate.sh script.
// DO NOT EDIT THIS FILE MANUALLY!

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::NoColor;
use codespan_reporting::term::{self, DisplayStyle};
use lelwel::frontend::parser::{tokenize, Parser, Token};
use lelwel::frontend::sema::SemanticPass;
use logos::Logos;
use std::io::BufWriter;

fn gen_diags(input: &str) -> String {
    let source = std::fs::read_to_string(input).unwrap();
    let mut diags = vec![];
    let (tokens, ranges) = tokenize(Token::lexer(&source), &mut diags);
    let cst = Parser::parse(&source, tokens, ranges, &mut diags);
    let _ = SemanticPass::run(&cst, &mut diags);

    let mut writer = NoColor::new(BufWriter::new(Vec::new()));
    let config = codespan_reporting::term::Config {
        display_style: DisplayStyle::Short,
        ..Default::default()
    };
    let file = SimpleFile::new(input, &source);
    for diag in diags {
        term::emit(&mut writer, &config, &file, &diag).unwrap();
    }
    std::str::from_utf8(writer.get_ref().buffer())
        .unwrap()
        .to_string()
}

#[test]
#[rustfmt::skip]
fn calc() {
    let diags = gen_diags("examples/calc/src/calc.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn c() {
    let diags = gen_diags("examples/c/src/c.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn empty() {
    let diags = gen_diags("tests/frontend/empty.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "error[E008]: missing start rule");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn invalid_node_create() {
    let diags = gen_diags("tests/frontend/invalid_node_create.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_node_create.llw:6:5: error[E024]: node marker is not visited before node creation");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn invalid_token() {
    let diags = gen_diags("tests/frontend/invalid_token.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_token.llw:6:5: error: invalid token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_token.llw:6:7: error: invalid token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_token.llw:6:8: error: invalid token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_token.llw:6:9: error: invalid token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/invalid_token.llw:6:11: error: invalid token");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn json() {
    let diags = gen_diags("examples/json/src/json.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn left_rec_elision() {
    let diags = gen_diags("tests/frontend/left_rec_elision.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/left_rec_elision.llw:6:3: error[E021]: left recursive rule branch cannot be elided");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn left_rec_node_create() {
    let diags = gen_diags("tests/frontend/left_rec_node_create.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/left_rec_node_create.llw:6:9: error[E025]: node creation without index is not allowed in left recursive rules");
    assert_eq!(lines.next().unwrap(), "tests/frontend/left_rec_node_create.llw:7:5: error[E025]: node creation without index is not allowed in left recursive rules");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn left_recursive() {
    let diags = gen_diags("tests/frontend/left_recursive.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/left_recursive.llw:6:5: error[E012]: LL(1) conflict in left recursive rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/left_recursive.llw:9:3: error[E011]: LL(1) conflict in alternation");
    assert_eq!(lines.next().unwrap(), "tests/frontend/left_recursive.llw:15:5: error[E012]: LL(1) conflict in left recursive rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/left_recursive.llw:13:3: error[E011]: LL(1) conflict in alternation");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn lelwel() {
    let diags = gen_diags("src/frontend/lelwel.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn ll1_conflict() {
    let diags = gen_diags("tests/frontend/ll1_conflict.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:6:4: error[E011]: LL(1) conflict in alternation");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:7:4: error[E011]: LL(1) conflict in alternation");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:8:3: error[E013]: LL(1) conflict in repetition");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:9:3: error[E013]: LL(1) conflict in repetition");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:10:3: error[E014]: LL(1) conflict in option");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:21:3: error[E014]: LL(1) conflict in option");
    assert_eq!(lines.next().unwrap(), "tests/frontend/ll1_conflict.llw:30:3: error[E011]: LL(1) conflict in alternation");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn l() {
    let diags = gen_diags("examples/l/src/l.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn lowercase_token() {
    let diags = gen_diags("tests/frontend/lowercase_token.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/lowercase_token.llw:1:7: error[E007]: token name starts with lower case letter");
    assert_eq!(lines.next().unwrap(), "tests/frontend/lowercase_token.llw:1:9: error[E007]: token name starts with lower case letter");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn lua() {
    let diags = gen_diags("examples/lua/src/lua.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn mixing_assoc() {
    let diags = gen_diags("tests/frontend/mixing_assoc.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/mixing_assoc.llw:7:8: error[E020]: mixed associativity in infix operator branch");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn node_rename() {
    let diags = gen_diags("tests/frontend/node_rename.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn oberon0() {
    let diags = gen_diags("examples/oberon0/src/oberon0.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn predef_token() {
    let diags = gen_diags("tests/frontend/predef_token.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/predef_token.llw:1:7: error[E010]: use of predefined token name");
    assert_eq!(lines.next().unwrap(), "tests/frontend/predef_token.llw:6:11: error[E004]: use of undefined token `EOF`");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn predicate_position() {
    let diags = gen_diags("tests/frontend/predicate_position.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/predicate_position.llw:6:3: error[E002]: invalid predicate position");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn redefinition() {
    let diags = gen_diags("tests/frontend/redefinition.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/redefinition.llw:3:6: error: invalid syntax, expected: <identifier>");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redefinition.llw:9:1: error[E005]: redefinition of rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redefinition.llw:14:1: error[E005]: redefinition of rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redefinition.llw:1:9: error[E005]: redefinition of token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redefinition.llw:1:13: error[E005]: redefinition of token");
    assert_eq!(lines.next().unwrap(), "error[E008]: missing start rule");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn redef_node_marker() {
    let diags = gen_diags("tests/frontend/redef_node_marker.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/redef_node_marker.llw:6:3: error[E022]: node marker was already defined");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn redundant_elision() {
    let diags = gen_diags("tests/frontend/redundant_elision.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/redundant_elision.llw:4:11: warning[W004]: node elision is redundant");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redundant_elision.llw:5:9: warning[W004]: node elision is redundant");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redundant_elision.llw:5:11: warning[W004]: node elision is redundant");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redundant_elision.llw:5:21: warning[W004]: node elision is redundant");
    assert_eq!(lines.next().unwrap(), "tests/frontend/redundant_elision.llw:1:13: warning[W002]: unused token");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn syntax_error() {
    let diags = gen_diags("tests/frontend/syntax_error.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:2:1: error: invalid syntax, expected one of: '=', <identifier>, ';'");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:8:1: error: invalid syntax, expected: ')'");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:13:2: error: invalid syntax, expected one of: <semantic action>, '^', <identifier>, '[', '(', <node creation>, <node marker>, <node rename>, '|', <semantic predicate>, ']', ')', ';', <string literal>");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:13:1: error[E003]: use of undefined rule `b`");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn undefined() {
    let diags = gen_diags("tests/frontend/undefined.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:3: error[E004]: use of undefined token `A`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:5: error[E003]: use of undefined rule `b`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:7: error[E004]: use of undefined token `C`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:9: error[E003]: use of undefined rule `d`");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn undef_node_create() {
    let diags = gen_diags("tests/frontend/undef_node_create.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/undef_node_create.llw:4:9: error[E023]: node creation with undefined node marker");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn unused_element() {
    let diags = gen_diags("tests/frontend/unused_element.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:9:1: warning[W001]: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:13:1: warning[W001]: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:23:1: warning[W001]: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:1:7: warning[W002]: unused token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:1:11: warning[W002]: unused token");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn unused_node_marker() {
    let diags = gen_diags("tests/frontend/unused_node_marker.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_node_marker.llw:4:7: warning[W003]: unused node marker");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn uppercase_rule() {
    let diags = gen_diags("tests/frontend/uppercase_rule.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/uppercase_rule.llw:6:1: error[E006]: rule name starts with upper case letter");
    assert_eq!(lines.next().unwrap(), "tests/frontend/uppercase_rule.llw:9:1: error[E006]: rule name starts with upper case letter");
    assert_eq!(lines.next(), None);
}
