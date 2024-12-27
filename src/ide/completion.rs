use tower_lsp::lsp_types::*;

use super::lookup::*;
use crate::frontend::ast::{AstNode, File, Named};
use crate::{Cst, Node, NodeRef, Rule, Token};

fn add_top_level_items(cst: &Cst, file: File, items: &mut Vec<CompletionItem>) {
    if file.start_decls(cst).count() == 0 {
        // only suggest if there is no start declaration already
        items.push(CompletionItem {
            label: "start rule_name;".to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            insert_text: Some("start ${1:rule_name};".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }
    items.push(CompletionItem {
        label: "token TokenName='token symbol';".to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        insert_text: Some("token ${1:TokenName}=${2:'token symbol'};".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });
    items.push(CompletionItem {
        label: "right TokenName;".to_string(),
        kind: Some(CompletionItemKind::KEYWORD),
        insert_text: Some("right ${1:TokenName};".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });
    items.push(CompletionItem {
        label: "rule_name: regex;".to_string(),
        insert_text: Some("${1:rule_name}: ${2:regex};".to_string()),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        ..Default::default()
    });
}

fn add_reference_items(
    cst: &Cst,
    file: File,
    items: &mut Vec<CompletionItem>,
    with_rules: bool,
    with_tokens: bool,
) {
    if with_rules {
        for rule in file.rule_decls(cst) {
            items.push(CompletionItem {
                label: rule.name(cst).unwrap().0.to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some("Rule".to_string()),
                    ..Default::default()
                }),
                kind: Some(CompletionItemKind::REFERENCE),
                ..Default::default()
            });
        }
    }
    if with_tokens {
        for token in file.token_decls(cst) {
            items.push(CompletionItem {
                label: token.name(cst).unwrap().0.to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some("Token".to_string()),
                    ..Default::default()
                }),
                kind: Some(CompletionItemKind::REFERENCE),
                ..Default::default()
            });
            if let Some(symbol) = token.symbol(cst) {
                items.push(CompletionItem {
                    label: symbol.0.to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: Some("Token".to_string()),
                        ..Default::default()
                    }),
                    kind: Some(CompletionItemKind::REFERENCE),
                    ..Default::default()
                });
            }
        }
    }
    if with_rules && with_tokens {
        items.push(CompletionItem {
            label: "?1".to_string(),
            label_details: Some(CompletionItemLabelDetails {
                description: Some("Semantic Predicate".to_string()),
                ..Default::default()
            }),
            kind: Some(CompletionItemKind::OPERATOR),
            insert_text: Some("?${1:1}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
        items.push(CompletionItem {
            label: "#1".to_string(),
            label_details: Some(CompletionItemLabelDetails {
                description: Some("Semantic Action".to_string()),
                ..Default::default()
            }),
            kind: Some(CompletionItemKind::OPERATOR),
            insert_text: Some("#${1:1}".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            ..Default::default()
        });
    }
}

pub fn completion(cst: &Cst, pos: usize) -> Option<CompletionResponse> {
    let mut items = vec![];

    let file = File::cast(cst, NodeRef::ROOT)?;
    if let Some(node) = lookup_node(cst, NodeRef::ROOT, pos) {
        match cst.get(node) {
            Node::Rule(
                Rule::Alternation
                | Rule::Concat
                | Rule::Star
                | Rule::Plus
                | Rule::Paren
                | Rule::Optional
                | Rule::Name
                | Rule::Symbol
                | Rule::Predicate
                | Rule::NodeRename
                | Rule::NodeMarker
                | Rule::NodeCreation
                | Rule::NodeElision,
                _,
            ) => {
                add_reference_items(cst, file, &mut items, true, true);
            }
            Node::Rule(Rule::RuleDecl, _) => {
                if let Some((_, range)) = cst
                    .children(node)
                    .find_map(|n| cst.get_token(n, Token::Colon))
                {
                    if pos > range.start {
                        add_reference_items(cst, file, &mut items, true, true);
                    }
                } else {
                    add_top_level_items(cst, file, &mut items);
                }
            }
            Node::Rule(Rule::SkipDecl | Rule::RightDecl, _) => {
                add_reference_items(cst, file, &mut items, false, true);
            }
            Node::Rule(Rule::StartDecl, _) => {
                add_reference_items(cst, file, &mut items, true, false);
            }
            Node::Rule(Rule::TokenList | Rule::TokenDecl, _) => {}
            _ => {
                add_top_level_items(cst, file, &mut items);
            }
        }
    } else {
        add_top_level_items(cst, file, &mut items);
    }
    Some(CompletionResponse::Array(items))
}
