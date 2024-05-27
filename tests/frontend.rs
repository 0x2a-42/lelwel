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
    let diags = gen_diags("tests/frontend/calc.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn c() {
    let diags = gen_diags("tests/frontend/c.llw");
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
    let diags = gen_diags("tests/frontend/json.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn lelwel() {
    let diags = gen_diags("tests/frontend/lelwel.llw");
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
    let diags = gen_diags("tests/frontend/l.llw");
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
    let diags = gen_diags("tests/frontend/lua.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn oberon0() {
    let diags = gen_diags("tests/frontend/oberon0.llw");
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
fn syntax_error() {
    let diags = gen_diags("tests/frontend/syntax_error.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:2:1: error: invalid syntax, expected one of: '=', <identifier>, ';'");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:8:1: error: invalid syntax, expected: ')'");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:13:2: error: invalid syntax, expected one of: <semantic action>, <binding>, <close node mark>, <identifier>, '[', '(', <open node mark>, '|', <semantic predicate>, ']', ')', ';', <string literal>");
    assert_eq!(lines.next().unwrap(), "tests/frontend/syntax_error.llw:13:1: error[E004]: use of undefined rule `b`");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn undefined() {
    let diags = gen_diags("tests/frontend/undefined.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:3: error[E004]: use of undefined token `A`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:5: error[E004]: use of undefined rule `b`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:7: error[E004]: use of undefined token `C`");
    assert_eq!(lines.next().unwrap(), "tests/frontend/undefined.llw:4:9: error[E004]: use of undefined rule `d`");
    assert_eq!(lines.next(), None);
}

#[test]
#[rustfmt::skip]
fn unused_element() {
    let diags = gen_diags("tests/frontend/unused_element.llw");
    let mut lines = diags.lines();

    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:9:1: warning: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:13:1: warning: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:23:1: warning: unused rule");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:1:7: warning: unused token");
    assert_eq!(lines.next().unwrap(), "tests/frontend/unused_element.llw:1:11: warning: unused token");
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
