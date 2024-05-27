use crate::frontend::parser::Span;
use crate::{Cst, Node, NodeRef, Rule, SemanticData};

fn contains(span: &Span, pos: usize) -> bool {
    span.start <= pos && pos < span.end
}

pub fn lookup_node(cst: &Cst, node: NodeRef, pos: usize) -> Option<NodeRef> {
    cst.children(node)
        .filter(|node| matches!(cst.get(*node), Node::Rule(..)))
        .find(|node| {
            cst.get_span(*node)
                .map_or(false, |span| contains(&span, pos))
        })
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
        .find(|node| {
            cst.get_span(*node)
                .map_or(false, |span| contains(&span, pos))
        })
        .and_then(|node| find_node(cst, node, pos, pred).or(Some(node)))
}

pub fn lookup_definition(cst: &Cst, sema: &SemanticData, pos: usize) -> Option<NodeRef> {
    lookup_node(cst, NodeRef::ROOT, pos).and_then(|node| sema.decl_bindings.get(&node).copied())
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
