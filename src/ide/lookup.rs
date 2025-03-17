use codespan_reporting::files::{Files, SimpleFile};
use tower_lsp::lsp_types::{Location, Url};

use crate::frontend::parser::Span;
use crate::{Cst, Node, NodeRef, Rule, SemanticData};

fn contains(span: &Span, pos: usize) -> bool {
    span.start <= pos && pos < span.end
}

pub fn lookup_node(cst: &Cst, node: NodeRef, pos: usize) -> Option<NodeRef> {
    cst.children(node)
        .filter(|node| matches!(cst.get(*node), Node::Rule(..)))
        .find(|node| cst.get_span(*node).is_some_and(|span| contains(&span, pos)))
        .and_then(|node| lookup_node(cst, node, pos).or(Some(node)))
}
pub fn find_node<P: Fn(Rule) -> bool>(
    cst: &Cst,
    node: NodeRef,
    pos: usize,
    pred: P,
) -> Option<NodeRef> {
    cst.children(node)
        .filter(|node| {
            if let Node::Rule(r, _) = cst.get(*node) {
                pred(r)
            } else {
                false
            }
        })
        .find(|node| cst.get_span(*node).is_some_and(|span| contains(&span, pos)))
        .and_then(|node| find_node(cst, node, pos, pred).or(Some(node)))
}

pub fn lookup_definition(
    cst: &Cst,
    sema: &SemanticData,
    pos: usize,
    uri: &Url,
    file: &SimpleFile<&str, &str>,
    parser_path: &std::path::Path,
) -> Option<Location> {
    lookup_node(cst, NodeRef::ROOT, pos).and_then(|node| {
        if let Some((rule_name, number)) = sema.predicates.get(&node) {
            if *number == "t" {
                return None;
            }
            lookup_parser_impl_definition("predicate", rule_name, number, parser_path)
        } else if let Some((rule_name, number)) = sema.actions.get(&node) {
            lookup_parser_impl_definition("action", rule_name, number, parser_path)
        } else {
            sema.decl_bindings
                .get(&node)
                .and_then(|node| cst.get_span(*node))
                .map(|span| Location {
                    uri: uri.clone(),
                    range: super::compat::span_to_range(file, &span),
                })
        }
    })
}

pub fn lookup_references(
    cst: &Cst,
    sema: &SemanticData,
    pos: usize,
    with_def: bool,
) -> Vec<NodeRef> {
    if let Some(def) = lookup_node(cst, NodeRef::ROOT, pos) {
        let mut refs = sema
            .decl_bindings
            .iter()
            .filter_map(|(k, v)| (*v == def).then_some(*k))
            .collect::<Vec<_>>();
        if with_def {
            refs.push(def)
        }
        refs
    } else {
        vec![]
    }
}

/// Heuristic search for predicate or action implementation in the parser.rs file.
/// This doesn't work if these definitions are moved to another file and it doesn't
/// consider comments or definitions outside of the Parser impl.
fn lookup_parser_impl_definition(
    kind: &str,
    rule_name: &str,
    number: &str,
    parser_path: &std::path::Path,
) -> Option<Location> {
    let uri = Url::from_file_path(parser_path).ok()?;
    let source = std::fs::read_to_string(parser_path).ok()?;
    let file = SimpleFile::new(parser_path.to_str()?, source.as_str());
    source
        .find(&format!("fn {kind}_{rule_name}_{number}"))
        .and_then(|offset| file.location((), offset).ok())
        .map(|loc| {
            let pos = tower_lsp::lsp_types::Position::new(
                (loc.line_number - 1) as u32,
                (loc.column_number - 1) as u32,
            );
            Location {
                uri,
                range: tower_lsp::lsp_types::Range::new(pos, pos),
            }
        })
}
