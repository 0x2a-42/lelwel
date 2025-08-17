use crate::{Cst, Node, NodeRef, Rule, Span};

#[allow(dead_code)]
pub trait AstNode {
    fn cast(cst: &Cst, syntax: NodeRef) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> NodeRef;

    fn span(&self, cst: &Cst) -> Span {
        cst.span(self.syntax())
    }
}

macro_rules! ast_node {
    ($node_name:ident) => {
        #[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
        pub struct $node_name {
            syntax: NodeRef,
        }
        impl AstNode for $node_name {
            fn cast(cst: &Cst, syntax: NodeRef) -> Option<Self> {
                match cst.get(syntax) {
                    Node::Rule(Rule::$node_name, _) => Some(Self { syntax }),
                    _ => None,
                }
            }
            fn syntax(&self) -> NodeRef {
                self.syntax
            }
        }
    };
    ($node_name:ident, ($($node_names:ident),+)) => {
        #[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
        pub enum $node_name {
            $($node_names($node_names),)*
        }
        impl AstNode for $node_name {
            fn cast(cst: &Cst, syntax: NodeRef) -> Option<Self> {
                $(
                if let Some(node) = $node_names::cast(cst, syntax) {
                    return Some(Self::$node_names(node));
                }
                )*
                return None;
            }
            fn syntax(&self) -> NodeRef {
                match self {
                    $(Self::$node_names(node) => node.syntax,)*
                }
            }
        }
    }
}

ast_node!(Binexp);
ast_node!(Unaryexp);
ast_node!(Literalexp);
ast_node!(Nameexp);
ast_node!(Parenexp);
ast_node!(Fieldexp);
ast_node!(Indexexp);
ast_node!(Callexp);
ast_node!(Functiondef);
ast_node!(Tableconstructor);
ast_node!(
    Exp,
    (
        Binexp,
        Unaryexp,
        Literalexp,
        Nameexp,
        Parenexp,
        Fieldexp,
        Indexexp,
        Callexp,
        Functiondef,
        Tableconstructor
    )
);
