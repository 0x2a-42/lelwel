use crate::lexer::Token;
use crate::{Cst, CstChildren, Node, NodeRef, Rule, Span};

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
        #[allow(clippy::enum_variant_names)]
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

ast_node!(DeclarationSpecifiers);
ast_node!(Declaration);
ast_node!(InitDeclaratorList);
ast_node!(InitDeclarator);
ast_node!(FunctionDefinition);
ast_node!(TypeSpecifier);
ast_node!(Declarator);
ast_node!(FunctionDeclarator);
ast_node!(ArrayDeclarator);
ast_node!(ParenDeclarator);
ast_node!(IdentDeclarator);
ast_node!(Enumerator);
ast_node!(
    DirectDeclarator,
    (
        FunctionDeclarator,
        ArrayDeclarator,
        ParenDeclarator,
        IdentDeclarator
    )
);

impl DeclarationSpecifiers {
    pub fn has_type_specifier(&self, cst: &Cst) -> bool {
        cst.children(self.syntax)
            .any(|node| TypeSpecifier::cast(cst, node).is_some())
    }
}
impl Declaration {
    pub fn declaration_specifiers(&self, cst: &Cst) -> Option<DeclarationSpecifiers> {
        cst.children(self.syntax)
            .find_map(|node| DeclarationSpecifiers::cast(cst, node))
    }
    pub fn init_declarator_list(&self, cst: &Cst) -> Option<InitDeclaratorList> {
        cst.children(self.syntax)
            .find_map(|node| InitDeclaratorList::cast(cst, node))
    }
}
impl InitDeclaratorList {
    pub fn init_declarators<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<InitDeclarator> + 'a>
    {
        cst.children(self.syntax)
            .filter_map(|c| InitDeclarator::cast(cst, c))
    }
}
impl InitDeclarator {
    pub fn declarator(&self, cst: &Cst) -> Option<Declarator> {
        cst.children(self.syntax)
            .find_map(|node| Declarator::cast(cst, node))
    }
}
impl FunctionDefinition {
    pub fn declaration_specifiers(&self, cst: &Cst) -> Option<DeclarationSpecifiers> {
        cst.children(self.syntax)
            .find_map(|node| DeclarationSpecifiers::cast(cst, node))
    }
    pub fn declarator(&self, cst: &Cst) -> Option<Declarator> {
        cst.children(self.syntax)
            .find_map(|node| Declarator::cast(cst, node))
    }
}
impl Declarator {
    pub fn name<'a>(&self, cst: &Cst<'a>) -> Option<(&'a str, Span)> {
        self.direct_declarator(cst).and_then(|decl| decl.name(cst))
    }
    pub fn direct_declarator(&self, cst: &Cst) -> Option<DirectDeclarator> {
        cst.children(self.syntax)
            .find_map(|node| DirectDeclarator::cast(cst, node))
    }
}
impl DirectDeclarator {
    pub fn name<'a>(&self, cst: &Cst<'a>) -> Option<(&'a str, Span)> {
        match self {
            DirectDeclarator::FunctionDeclarator(decl) => {
                decl.base(cst).and_then(|decl| decl.name(cst))
            }
            DirectDeclarator::ArrayDeclarator(decl) => {
                decl.base(cst).and_then(|decl| decl.name(cst))
            }
            DirectDeclarator::ParenDeclarator(decl) => {
                decl.base(cst).and_then(|decl| decl.name(cst))
            }
            DirectDeclarator::IdentDeclarator(decl) => decl.name(cst),
        }
    }
}
impl FunctionDeclarator {
    pub fn base(&self, cst: &Cst) -> Option<DirectDeclarator> {
        cst.children(self.syntax)
            .find_map(|node| DirectDeclarator::cast(cst, node))
    }
    pub fn is_complete(&self, cst: &Cst) -> bool {
        cst.children(self.syntax)
            .find_map(|node| cst.match_token(node, Token::RPar))
            .is_some()
    }
}
impl ArrayDeclarator {
    pub fn base(&self, cst: &Cst) -> Option<DirectDeclarator> {
        cst.children(self.syntax)
            .find_map(|node| DirectDeclarator::cast(cst, node))
    }
}
impl ParenDeclarator {
    pub fn base(&self, cst: &Cst) -> Option<Declarator> {
        cst.children(self.syntax)
            .find_map(|node| Declarator::cast(cst, node))
    }
}
impl IdentDeclarator {
    pub fn name<'a>(&self, cst: &Cst<'a>) -> Option<(&'a str, Span)> {
        cst.children(self.syntax)
            .find_map(|node| cst.match_token(node, Token::Identifier))
    }
}
impl Enumerator {
    pub fn name<'a>(&self, cst: &Cst<'a>) -> Option<(&'a str, Span)> {
        cst.children(self.syntax)
            .find_map(|node| cst.match_token(node, Token::Identifier))
    }
}
