use tower_lsp::lsp_types::*;

use super::lookup::*;
use crate::frontend::ast::{AstNode, File, Name, Named};
use crate::{Cst, Node, NodeRef, Rule, SemanticData, Token};

/// Suggests a token name for a given token symbol.
fn symbol_name_suggestion(symbol: &str) -> String {
    let mut name = String::new();
    let mut it = symbol.chars();
    // remove quotation marks from symbol string
    it.next();
    it.next_back();

    if it.clone().any(|c| !c.is_ascii_alphanumeric() && c != '_') {
        let mut last = None;
        let mut last_count = 1;
        let mut escape = false;
        for c in it {
            if c == '\\' && !escape {
                escape = true;
                continue;
            }
            escape = false;
            if last == Some(c) {
                last_count += 1;
                continue;
            }
            if last_count > 1 {
                name.push_str(&last_count.to_string());
                last_count = 1;
            }
            last = Some(c);
            match c {
                '!' => name.push_str("Excl"),
                '"' => name.push_str("DQuote"),
                '#' => name.push_str("Hash"),
                '$' => name.push_str("Dollar"),
                '%' => name.push_str("Percent"),
                '&' => name.push_str("And"),
                '\'' => name.push_str("SQuote"),
                '(' => name.push_str("LPar"),
                ')' => name.push_str("RPar"),
                '*' => name.push_str("Star"),
                '+' => name.push_str("Plus"),
                ',' => name.push_str("Comma"),
                '-' => name.push_str("Minus"),
                '.' => name.push_str("Dot"),
                '/' => name.push_str("Slash"),
                '0' => name.push_str("Zero"),
                '1' => name.push_str("One"),
                '2' => name.push_str("Two"),
                '3' => name.push_str("Three"),
                '4' => name.push_str("Four"),
                '5' => name.push_str("Five"),
                '6' => name.push_str("Six"),
                '7' => name.push_str("Seven"),
                '8' => name.push_str("Eight"),
                '9' => name.push_str("Nine"),
                ':' => name.push_str("Colon"),
                ';' => name.push_str("Semi"),
                '<' => name.push_str("Lt"),
                '=' => name.push_str("Eq"),
                '>' => name.push_str("Gt"),
                '?' => name.push_str("Quest"),
                '@' => name.push_str("At"),
                'A'..='Z' => name.push(c),
                '[' => name.push_str("LBrak"),
                '\\' => name.push_str("BSlash"),
                ']' => name.push_str("RBrak"),
                '^' => name.push_str("Caret"),
                '_' => name.push_str("UScore"),
                '`' => name.push_str("BTick"),
                'a'..='z' => name.push(c.to_ascii_uppercase()),
                '{' => name.push_str("LBrace"),
                '|' => name.push_str("Pipe"),
                '}' => name.push_str("RBrace"),
                '~' => name.push_str("Tilde"),
                c => name.push_str(&format!("Symbol{}", c as u32)),
            }
        }
        if last_count > 1 {
            name.push_str(&last_count.to_string());
        }
    } else {
        // convert ascii snake case to pascal case
        let mut after_underscore = true;
        for c in it {
            if c == '_' {
                after_underscore = true;
                continue;
            }
            if after_underscore {
                name.push(c.to_ascii_uppercase());
                after_underscore = false;
            } else {
                name.push(c);
            }
        }
    }
    name
}

fn token_name_symbol(name_or_symbol: &str) -> Option<(String, &str)> {
    if name_or_symbol == "''" {
        return None;
    }
    if name_or_symbol.starts_with('\'') {
        Some((symbol_name_suggestion(name_or_symbol), name_or_symbol))
    } else {
        Some((name_or_symbol.to_string(), ""))
    }
}

fn add_top_level_items(
    cst: &Cst,
    file: File,
    sema: &SemanticData,
    items: &mut Vec<CompletionItem>,
) {
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
    for rule_name in sema.undefined_rules.iter() {
        items.push(CompletionItem {
            label: format!("{rule_name}:;"),
            label_details: Some(CompletionItemLabelDetails {
                description: Some("Rule".to_string()),
                ..Default::default()
            }),
            ..Default::default()
        });
    }
    for token_name_or_symbol in sema.undefined_tokens.iter() {
        if let Some((token_name, symbol_name)) = token_name_symbol(token_name_or_symbol) {
            items.push(CompletionItem {
                label: format!(
                    "token {token_name}{}{symbol_name};",
                    if symbol_name.is_empty() { "" } else { "=" }
                ),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some("Token".to_string()),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
    }
}

fn add_token(sema: &SemanticData, items: &mut Vec<CompletionItem>) {
    for token_name_or_symbol in sema.undefined_tokens.iter() {
        if let Some((token_name, symbol_name)) = token_name_symbol(token_name_or_symbol) {
            items.push(CompletionItem {
                label: format!(
                    "{token_name}{}{symbol_name}",
                    if symbol_name.is_empty() { "" } else { "=" }
                ),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some("Token".to_string()),
                    ..Default::default()
                }),
                ..Default::default()
            });
        }
    }
}

fn add_reference_items(
    cst: &Cst,
    file: File,
    sema: &SemanticData,
    items: &mut Vec<CompletionItem>,
    with_rules: bool,
    with_tokens: bool,
    ignore: Option<&str>,
) {
    if with_rules {
        for rule in file.rule_decls(cst) {
            let name = rule.name(cst).unwrap().0;
            if name.starts_with(|c: char| c.is_uppercase()) {
                // some syntax errors may cause token references
                // to be parsed as rule declarations
                continue;
            }
            items.push(CompletionItem {
                label: name.to_string(),
                label_details: Some(CompletionItemLabelDetails {
                    description: Some("Rule".to_string()),
                    ..Default::default()
                }),
                kind: Some(CompletionItemKind::REFERENCE),
                ..Default::default()
            });
        }
        for rule_name in sema.undefined_rules.iter() {
            if ignore != Some(rule_name) {
                items.push(CompletionItem {
                    label: rule_name.to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: Some("Rule".to_string()),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
            }
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
        for token_name_or_symbol in sema.undefined_tokens.iter() {
            if ignore != Some(token_name_or_symbol) {
                items.push(CompletionItem {
                    label: token_name_or_symbol.to_string(),
                    label_details: Some(CompletionItemLabelDetails {
                        description: Some("Token".to_string()),
                        ..Default::default()
                    }),
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

fn inside_decl(cst: &Cst, node: NodeRef, pos: usize) -> bool {
    cst.children(node)
        .find_map(|n| cst.get_token(n, Token::Semi))
        .map_or(true, |(_, range)| pos < range.end)
}

pub fn completion(cst: &Cst, pos: usize, sema: &SemanticData) -> Option<CompletionResponse> {
    let mut items = vec![];

    let file = File::cast(cst, NodeRef::ROOT)?;
    if let Some(node) = lookup_node(cst, NodeRef::ROOT, pos.saturating_sub(1)) {
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
                | Rule::NodeElision,
                _,
            ) => {
                add_reference_items(
                    cst,
                    file,
                    sema,
                    &mut items,
                    true,
                    true,
                    Name::cast(cst, node)
                        .and_then(|name_ref| name_ref.value(cst))
                        .map(|(name, _)| name),
                );
            }
            Node::Rule(Rule::NodeCreation | Rule::NodeRename, _) => {
                add_reference_items(cst, file, sema, &mut items, true, false, None);
            }
            Node::Rule(Rule::RuleDecl, _) => {
                if inside_decl(cst, node, pos) {
                    if let Some((_, range)) = cst
                        .children(node)
                        .find_map(|n| cst.get_token(n, Token::Colon))
                    {
                        if pos > range.start {
                            add_reference_items(cst, file, sema, &mut items, true, true, None);
                        }
                    } else {
                        add_top_level_items(cst, file, sema, &mut items);
                    }
                } else {
                    add_top_level_items(cst, file, sema, &mut items);
                }
            }
            Node::Rule(Rule::SkipDecl | Rule::RightDecl, _) => {
                if inside_decl(cst, node, pos) {
                    add_reference_items(cst, file, sema, &mut items, false, true, None);
                } else {
                    add_top_level_items(cst, file, sema, &mut items);
                }
            }
            Node::Rule(Rule::StartDecl, _) => {
                if inside_decl(cst, node, pos) {
                    add_reference_items(cst, file, sema, &mut items, true, false, None);
                } else {
                    add_top_level_items(cst, file, sema, &mut items);
                }
            }
            Node::Rule(Rule::TokenList | Rule::TokenDecl, _) => {
                if inside_decl(cst, node, pos) {
                    add_token(sema, &mut items);
                } else {
                    add_top_level_items(cst, file, sema, &mut items);
                }
            }
            Node::Rule(Rule::File, _) => {
                add_top_level_items(cst, file, sema, &mut items);
            }
            _ => {}
        }
    } else {
        add_top_level_items(cst, file, sema, &mut items);
    }
    Some(CompletionResponse::Array(items))
}
