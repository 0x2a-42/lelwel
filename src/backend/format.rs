#![cfg(any(feature = "lsp", feature = "cli"))]

use crate::{
    Cst, NodeRef, Span,
    frontend::{
        ast::{self, AstNode},
        lexer::Token,
        parser::{Node, Rule},
    },
};
use dprint_core::formatting::*;
use std::rc::Rc;

pub fn format(cst: &Cst<'_>) -> String {
    dprint_core::formatting::format(
        || gen_cst(cst),
        PrintOptions {
            indent_width: 1,
            max_width: 100,
            use_tabs: false,
            new_line_text: "\n",
        },
    )
}

fn gen_cst(cst: &Cst<'_>) -> PrintItems {
    let mut items = PrintItems::new();
    gen_node(cst, NodeRef::ROOT, &mut items);
    items
}

fn gen_node(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    match cst.get(node_ref) {
        Node::Rule(Rule::Action, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Alternation, _) => gen_alt(cst, node_ref, items),
        Node::Rule(Rule::Assertion, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Commit, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Concat, _) => gen_concat(cst, node_ref, items),
        Node::Rule(Rule::Decl, _) => unreachable!(),
        Node::Rule(Rule::Error, _) => gen_error(cst, node_ref, items),
        Node::Rule(Rule::File, _) => gen_file(cst, node_ref, items),
        Node::Rule(Rule::Name, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::NodeCreation, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::NodeElision, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::NodeMarker, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::NodeRename, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Optional, _) => gen_paren(cst, node_ref, items),
        Node::Rule(Rule::OrderedChoice, _) => gen_alt(cst, node_ref, items),
        Node::Rule(Rule::Paren, _) => gen_paren(cst, node_ref, items),
        Node::Rule(Rule::PartDecl, _) => gen_semi_list(cst, node_ref, items),
        Node::Rule(Rule::Plus, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Postfix, _) => unreachable!(),
        Node::Rule(Rule::Predicate, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::Regex, _) => unreachable!(),
        Node::Rule(Rule::Return, _) => gen_rule_decl(cst, node_ref, items),
        Node::Rule(Rule::RightDecl, _) => gen_semi_list(cst, node_ref, items),
        Node::Rule(Rule::RuleDecl, _) => gen_rule_decl(cst, node_ref, items),
        Node::Rule(Rule::SkipDecl, _) => gen_semi_list(cst, node_ref, items),
        Node::Rule(Rule::Star, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::StartDecl, _) => gen_semi_list(cst, node_ref, items),
        Node::Rule(Rule::Symbol, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::TokenDecl, _) => gen_children(cst, node_ref, items),
        Node::Rule(Rule::TokenList, _) => gen_semi_list(cst, node_ref, items),
        Node::Token(token, idx) => {
            let span = cst.span(node_ref);
            let txt = cst.span_text(idx);
            match token {
                Token::LineComment | Token::DocComment => {
                    space_before_comment(cst, &span, items, false);
                    items.push_string(txt[..txt.len() - 1].to_string());
                    items.push_signal(Signal::ExpectNewLine);
                }
                Token::BlockComment => {
                    space_before_comment(cst, &span, items, false);
                    items.push_string(txt.to_string());
                }
                Token::Whitespace => {}
                _ => items.push_string(txt.to_string()),
            }
        }
    }
}

fn indent(width: usize, items: &mut PrintItems) {
    for _ in 0..width {
        items.push_signal(Signal::StartIndent);
    }
}

fn dedent(width: usize, items: &mut PrintItems) {
    for _ in 0..width {
        items.push_signal(Signal::FinishIndent);
    }
}

fn space_before_comment(cst: &Cst<'_>, span: &Span, items: &mut PrintItems, global: bool) {
    for c in cst.source()[..span.start].bytes().rev() {
        match c {
            b' ' => {}
            b'\n' => {
                if !global {
                    items.push_signal(Signal::NewLine);
                }
                return;
            }
            _ => {
                items.push_space();
                return;
            }
        }
    }
}

fn new_line_if_multiple_lines(start_ln: LineNumber, end_ln: LineNumber) -> Condition {
    conditions::if_true(
        "newLineIfMultipleLines",
        Rc::new(move |context| condition_helpers::is_multiple_lines(context, start_ln, end_ln)),
        Signal::NewLine.into(),
    )
}

fn gen_concat(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    items.push_signal(Signal::StartNewLineGroup);
    let mut children = cst.children(node_ref);
    if let Some(first) = children.next() {
        gen_node(cst, first, items);
        for child_node_ref in children {
            match cst.get(child_node_ref) {
                Node::Token(Token::Whitespace, _) => {}
                Node::Token(Token::LineComment | Token::DocComment | Token::BlockComment, _) => {
                    gen_node(cst, child_node_ref, items);
                }
                _ => {
                    items.push_signal(Signal::SpaceOrNewLine);
                    gen_node(cst, child_node_ref, items);
                }
            }
        }
    }
    items.push_signal(Signal::FinishNewLineGroup);
}

fn gen_paren(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    let start_ln = LineNumber::new("start");
    let end_ln = LineNumber::new("end");
    items.push_info(start_ln);
    items.push_anchor(LineNumberAnchor::new(end_ln));

    for child_node_ref in cst.children(node_ref) {
        match cst.get(child_node_ref) {
            Node::Token(Token::LPar | Token::LBrak, _) => {
                gen_node(cst, child_node_ref, items);
                indent(2, items);
                items.push_condition(new_line_if_multiple_lines(start_ln, end_ln));
            }
            Node::Token(Token::RPar | Token::RBrak, _) => {
                dedent(2, items);
                items.push_condition(new_line_if_multiple_lines(start_ln, end_ln));
                gen_node(cst, child_node_ref, items);
            }
            _ => gen_node(cst, child_node_ref, items),
        }
    }
    items.push_info(end_ln);
}

fn gen_alt(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    let children = cst.children(node_ref).collect::<Vec<_>>();
    let regexes = children
        .iter()
        .filter(|child| ast::Regex::cast(cst, **child).is_some())
        .copied()
        .collect::<Vec<_>>();
    let force_multiline = if regexes.len() > 1 {
        let first_span = cst.span(regexes[0]);
        let second_span = cst.span(regexes[1]);
        cst.source()[first_span.end..second_span.start].contains('\n')
    } else {
        false
    };

    let start_ln = LineNumber::new("start");
    let end_ln = LineNumber::new("end");
    let is_multiple_lines = Rc::new(
        move |condition_context: &mut ConditionResolverContext<'_, '_>| {
            condition_helpers::is_multiple_lines(condition_context, start_ln, end_ln)
                .map(|m| m || force_multiline)
        },
    );
    items.push_info(start_ln);
    items.push_anchor(LineNumberAnchor::new(end_ln));

    items.push_signal(Signal::StartNewLineGroup);
    let mut it = children.into_iter();
    if let Some(first) = it.next() {
        gen_node(cst, first, items);
        for child_node_ref in it {
            match cst.get(child_node_ref) {
                Node::Token(Token::Or | Token::Slash, _) => {
                    items.push_condition(conditions::if_true_or(
                        "multilineAlt",
                        is_multiple_lines.clone(),
                        {
                            let mut items = PrintItems::new();
                            items.push_signal(Signal::NewLine);
                            dedent(2, &mut items);
                            items
                        },
                        Signal::SpaceOrNewLine.into(),
                    ));
                    gen_node(cst, child_node_ref, items);
                    items.push_condition(conditions::if_true(
                        "multilineAlt",
                        is_multiple_lines.clone(),
                        {
                            let mut items = PrintItems::new();
                            indent(2, &mut items);
                            items
                        },
                    ));
                    items.push_space();
                }
                _ => {
                    gen_node(cst, child_node_ref, items);
                }
            }
        }
    }
    items.push_signal(Signal::FinishNewLineGroup);
    items.push_info(end_ln);
}

fn gen_children(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    for child_node_ref in cst.children(node_ref) {
        gen_node(cst, child_node_ref, items);
    }
}

fn gen_error(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    for child_node_ref in cst.children(node_ref) {
        match cst.get(child_node_ref) {
            Node::Token(Token::Whitespace, _) => items.push_space(),
            _ => gen_node(cst, child_node_ref, items),
        }
    }
}

fn gen_file(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    let mut line_start = true;
    for child_node_ref in cst.children(node_ref) {
        match cst.get(child_node_ref) {
            Node::Token(Token::LineComment | Token::DocComment, idx) => {
                let span = cst.span(child_node_ref);
                let txt = cst.span_text(idx);
                space_before_comment(cst, &span, items, true);
                items.push_string(txt[..txt.len() - 1].to_string());
                items.push_signal(Signal::NewLine);
                line_start = true;
            }
            Node::Token(Token::BlockComment, idx) => {
                let span = cst.span(child_node_ref);
                let txt = cst.span_text(idx);
                space_before_comment(cst, &span, items, true);
                items.push_string(txt.to_string());
                items.push_signal(Signal::SpaceIfNotTrailing);
            }
            Node::Token(Token::Whitespace, idx) => {
                let txt = cst.span_text(idx);
                let nl_count = txt.bytes().filter(|b| *b == b'\n').count();
                if nl_count > 0 {
                    for _ in 0..nl_count {
                        items.push_signal(Signal::NewLine);
                    }
                    line_start = true;
                } else {
                    line_start = false;
                }
            }
            _ => {
                if !line_start && ast::Decl::cast(cst, child_node_ref).is_some() {
                    items.push_signal(Signal::ExpectNewLine);
                }
                gen_node(cst, child_node_ref, items);
            }
        }
    }
}

fn gen_semi_list(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    let mut children = cst.children(node_ref);
    if let Some(first) = children.next() {
        gen_node(cst, first, items);
        let width = cst.span(first).len() + 1;
        indent(width, items);
        let mut first = true;
        for child_node_ref in children {
            match cst.get(child_node_ref) {
                Node::Token(Token::Whitespace, _) => {}
                Node::Token(Token::Semi, _) => gen_node(cst, child_node_ref, items),
                _ => {
                    if first {
                        items.push_space();
                    } else {
                        items.push_signal(Signal::SpaceOrNewLine);
                    }
                    gen_node(cst, child_node_ref, items);
                    first = false;
                }
            }
        }
        dedent(width, items);
    }
}

fn gen_rule_decl(cst: &Cst<'_>, node_ref: NodeRef, items: &mut PrintItems) {
    let mut children = cst.children(node_ref);
    if let Some(first) = children.next() {
        gen_node(cst, first, items);

        let start_ln = LineNumber::new("start");
        let end_ln = LineNumber::new("end");
        items.push_info(start_ln);
        items.push_anchor(LineNumberAnchor::new(end_ln));

        let mut semi_cond = new_line_if_multiple_lines(start_ln, end_ln);
        let semi_cond_reeval = semi_cond.create_reevaluation();
        for child_node_ref in children {
            match cst.get(child_node_ref) {
                Node::Token(Token::Colon, _) => {
                    gen_node(cst, child_node_ref, items);
                    indent(2, items);
                    items.push_condition(
                        conditions::new_line_if_multiple_lines_space_or_new_line_otherwise(
                            start_ln,
                            Some(end_ln),
                        ),
                    );
                }
                Node::Token(Token::Semi, _) => {
                    dedent(2, items);
                    items.push_condition(semi_cond.clone());
                    gen_node(cst, child_node_ref, items);
                }
                _ => gen_node(cst, child_node_ref, items),
            }
        }
        items.push_info(end_ln);
        items.push_reevaluation(semi_cond_reeval);
    }
}
