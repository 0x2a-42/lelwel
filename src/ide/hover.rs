use crate::frontend::ast::{AstNode, Regex, RuleDecl};
use crate::frontend::lexer::Token;
use crate::{Cst, NodeRef, Rule, SemanticData};
use logos::Span;

use super::lookup::*;

pub fn hover(cst: &Cst<'_>, sema: &SemanticData<'_>, pos: usize) -> Option<(String, Span)> {
    let node = lookup_rule_node(cst, NodeRef::ROOT, pos)?;
    let span = cst.span(node);

    if let Some(regex) = Regex::cast(cst, node) {
        let first = &sema
            .first_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let follow = &sema
            .follow_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let predict = &sema
            .predict_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));

        match regex {
            Regex::Star(_) | Regex::Plus(_) => {
                let recovery = &sema
                    .recovery_sets
                    .get(&regex.syntax())
                    .map_or("{}".to_string(), |s| format!("{s:?}"));
                Some((
                    format!(
                        "**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n**Recovery:** {recovery}\n"
                    ),
                    span,
                ))
            }
            Regex::Name(_) | Regex::Symbol(_) => {
                let comment_attached_node = sema.decl_bindings.get(&node).and_then(|decl| {
                    find_node(cst, NodeRef::ROOT, cst.span(*decl).start, |r| {
                        r == Rule::TokenList || r == Rule::RuleDecl
                    })
                });
                let mut comment_nodes = vec![];
                if let Some(comment_attached_node) = comment_attached_node {
                    for i in 1.. {
                        if let Some(node) = cst.match_token(
                            NodeRef(comment_attached_node.0.saturating_sub(i)),
                            Token::DocComment,
                        ) {
                            comment_nodes.push(node);
                        } else {
                            break;
                        }
                    }
                }
                let mut comment = String::new();
                for (val, _) in comment_nodes.iter().rev() {
                    comment.push_str(val.strip_prefix("///").unwrap().trim_start());
                }
                if !comment.is_empty() {
                    comment.push_str("---\n")
                }
                Some((
                    format!(
                        "{comment}**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"
                    ),
                    span,
                ))
            }
            _ => Some((
                format!("**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"),
                span,
            )),
        }
    } else if let Some(rule) = RuleDecl::cast(cst, node) {
        let regex = rule.regex(cst)?;
        let first = &sema
            .first_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let follow = &sema
            .follow_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        let predict = &sema
            .predict_sets
            .get(&regex.syntax())
            .map_or("{}".to_string(), |s| format!("{s:?}"));
        Some((
            format!("**First:** {first}\n**Follow:** {follow}\n**Predict:** {predict}\n"),
            span,
        ))
    } else {
        None
    }
}
