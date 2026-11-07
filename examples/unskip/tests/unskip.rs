use lelwel_unskip::lexer::Token;
use lelwel_unskip::parser::{Cst, Node, NodeRef, Parser, Rule};

fn descendants(cst: &Cst<'_>, node: NodeRef, nodes: &mut Vec<NodeRef>) {
    nodes.push(node);
    if matches!(cst.get(node), Node::Rule(..)) {
        for child in cst.children(node) {
            descendants(cst, child, nodes);
        }
    }
}

fn assert_valid<'a>(source: &'a str, terminators: &[&str]) -> Cst<'a> {
    let mut diags = vec![];
    let cst = Parser::new(source, &mut diags).parse(&mut diags);
    assert!(diags.is_empty(), "{source:?}: {diags:#?}\n{cst}");

    let mut nodes = vec![];
    descendants(&cst, NodeRef::ROOT, &mut nodes);
    let actual_terminators: Vec<_> = nodes
        .iter()
        .filter(|&&node| cst.match_rule(node, Rule::Terminator))
        .map(|&node| {
            let tokens: Vec<_> = cst
                .children(node)
                .filter(|&child| {
                    cst.match_token(child, Token::Newline).is_some()
                        || cst.match_token(child, Token::Comma).is_some()
                })
                .collect();
            assert_eq!(tokens.len(), 1, "{source:?}:\n{cst}");
            &source[cst.span(tokens[0])]
        })
        .collect();
    assert_eq!(actual_terminators, terminators, "{source:?}:\n{cst}");

    let mut offset = 0;
    for node in nodes {
        if matches!(cst.get(node), Node::Token(..)) {
            let span = cst.span(node);
            assert_eq!(span.start, offset, "{source:?}:\n{cst}");
            offset = span.end;
        }
    }
    assert_eq!(offset, source.len(), "{source:?}:\n{cst}");
    cst
}

#[test]
fn required_token_followed_by_unskipped_newline() {
    for source in ["single a\n", "single a \t\n", "single a\r\n"] {
        let terminator = if source.ends_with("\r\n") {
            "\r\n"
        } else {
            "\n"
        };
        assert_valid(source, &[terminator]);
    }
}

#[test]
fn non_skipped_terminator_still_works() {
    assert_valid("single a ,", &[","]);
}

#[test]
fn optional_token_can_be_present_or_absent() {
    for source in ["optional a\n", "optional\n", "optional \t\n"] {
        assert_valid(source, &["\n"]);
    }
}

#[test]
fn zero_or_more_stops_at_unskipped_newline() {
    for source in ["star\n", "star a\n", "star a a a \t\n"] {
        assert_valid(source, &["\n"]);
    }
}

#[test]
fn one_or_more_stops_at_unskipped_newline() {
    for source in ["plus a\n", "plus a a a \t\n"] {
        assert_valid(source, &["\n"]);
    }
}

#[test]
fn only_the_last_token_in_a_sequence_unskips_newline() {
    assert_valid("sequence a\n b\n c \t\n", &["\n"]);
}

#[test]
fn follow_set_propagates_through_a_nullable_rule() {
    for source in ["nested\n", "nested a b c\n", "nested a\n b\n c \t\n"] {
        assert_valid(source, &["\n"]);
    }
}

#[test]
fn either_of_two_skipped_tokens_can_be_unskipped() {
    for source in ["multiple a\nend", "multiple a end", "multiple a\t end"] {
        let cst = assert_valid(source, &[]);
        let mut nodes = vec![];
        descendants(&cst, NodeRef::ROOT, &mut nodes);
        let multiple = nodes
            .into_iter()
            .find(|&node| cst.match_rule(node, Rule::Multiple))
            .unwrap();
        let children: Vec<_> = cst.children(multiple).collect();
        let a = children
            .iter()
            .position(|&node| cst.match_token(node, Token::A).is_some())
            .unwrap();
        assert!(matches!(
            cst.get(children[a + 1]),
            Node::Token(Token::Newline | Token::Whitespace, _)
        ));
        assert!(cst.match_token(children[a + 2], Token::End).is_some());
    }
}

#[test]
fn skipped_terminator_in_repetition_accepts_blank_lines() {
    assert_valid("lines a\n\nend", &["\n", "\n"]);
    assert_valid("lines a\n \t\n\nend", &["\n", "\n", "\n"]);
    assert_valid("lines a\na\n\nend", &["\n", "\n", "\n"]);
}

#[test]
fn skipped_terminator_in_repetition_without_terminators() {
    for source in ["lines end", "lines\n\nend", "lines a end"] {
        assert_valid(source, &[]);
    }
}

#[test]
fn skip_tokens_remain_skipped_outside_exceptional_rules() {
    assert_valid("\n \tnormal\n a \t\n b\n c\n , \t\n", &[]);
}

#[test]
#[ignore = "known limitation: unskip lists use rule-wide follow sets, so a blank line after `single` leaks into `document`"]
fn unskip_does_not_leak_between_statements() {
    assert_valid(
        "single a\n\nnormal a\nb\nc,\noptional\nstar a a\nplus a\nnested\n",
        &["\n", "\n", "\n", "\n", "\n"],
    );
}

#[test]
fn missing_required_tokens_are_diagnosed() {
    for source in [
        "single a",
        "optional a",
        "star a",
        "plus a",
        "sequence a b c",
        "nested a b c",
        "plus\n",
        "multiple aend",
        "multiple a,end",
        "lines a",
        "lines a\n",
    ] {
        let mut diags = vec![];
        Parser::new(source, &mut diags).parse(&mut diags);
        assert!(!diags.is_empty(), "expected a diagnostic for {source:?}");
        assert!(
            diags.iter().all(|diag| diag.message != "invalid token"),
            "expected parser diagnostics, not lexer errors: {source:?}: {diags:#?}"
        );
    }
}
