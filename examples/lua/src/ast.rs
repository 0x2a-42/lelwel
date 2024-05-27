use crate::{Cst, Node, NodeRef, Rule, Span};

#[allow(dead_code)]
pub trait AstNode {
    fn cast(cst: &Cst, syntax: NodeRef) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> NodeRef;

    fn span(&self, cst: &Cst) -> Span {
        cst.get_span(self.syntax()).unwrap()
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

ast_node!(Chunk);
ast_node!(Block);
ast_node!(Emptystat);
ast_node!(Expstat);
ast_node!(Assignstat);
ast_node!(Label);
ast_node!(Breakstat);
ast_node!(Gotostat);
ast_node!(Dostat);
ast_node!(Whilestat);
ast_node!(Repeatstat);
ast_node!(Ifstat);
ast_node!(Forstat);
ast_node!(Funcstat);
ast_node!(Localstat);
ast_node!(Attrib);
ast_node!(Retstat);
ast_node!(
    Stat,
    (
        Emptystat, Expstat, Assignstat, Label, Breakstat, Gotostat, Dostat, Whilestat, Repeatstat,
        Ifstat, Forstat, Funcstat, Localstat, Retstat
    )
);
ast_node!(Binexp);
ast_node!(Unaryexp);
ast_node!(Powexp);
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
        Powexp,
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
