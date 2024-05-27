use crate::{Cst, CstChildren, Node, NodeRef, Rule, Span, Token};

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
    ($node_name:ident, $rule_name:ident, $token_name:ident) => {
        #[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
        pub struct $node_name {
            syntax: NodeRef,
        }
        impl AstNode for $node_name {
            fn cast(cst: &Cst, syntax: NodeRef) -> Option<Self> {
                match cst.get(syntax) {
                    Node::Rule(Rule::$rule_name, _)
                        if cst
                            .children(syntax)
                            .find_map(|n| cst.get_token(n, Token::$token_name))
                            .is_some() =>
                    {
                        Some(Self { syntax })
                    }
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

ast_node!(File);
ast_node!(TokenDecl);
ast_node!(RuleDecl);
ast_node!(StartDecl);
ast_node!(RightDecl);
ast_node!(SkipDecl);
ast_node!(
    Regex,
    (
        Alternation,
        Concat,
        Paren,
        Optional,
        Star,
        Plus,
        Name,
        Symbol,
        Predicate,
        Action,
        Binding,
        OpenNode,
        CloseNode
    )
);
ast_node!(Alternation);
ast_node!(Concat);
ast_node!(Paren);
ast_node!(Optional);
ast_node!(Star, Postfix, Star);
ast_node!(Plus, Postfix, Plus);
ast_node!(Name, Atomic, Id);
ast_node!(Symbol, Atomic, Str);
ast_node!(Predicate, Atomic, Predicate);
ast_node!(Action, Atomic, Action);
ast_node!(Binding, Atomic, Binding);
ast_node!(OpenNode, Atomic, OpenNode);
ast_node!(CloseNode, Atomic, CloseNode);

impl Cst<'_> {
    fn child_node<T: AstNode>(&self, syntax: NodeRef) -> Option<T> {
        self.children(syntax).find_map(|c| T::cast(self, c))
    }
    fn child_node_iter<T: AstNode>(
        &self,
        syntax: NodeRef,
    ) -> std::iter::FilterMap<CstChildren, impl FnMut(NodeRef) -> Option<T> + '_> {
        self.children(syntax).filter_map(|c| T::cast(self, c))
    }
    fn child_token(&self, syntax: NodeRef, token: Token) -> Option<(&str, Span)> {
        self.children(syntax).find_map(|c| self.get_token(c, token))
    }
}

pub trait Named: AstNode {
    fn name<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)>;
}
impl File {
    pub fn token_decls<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<
        std::iter::Flatten<
            std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<CstChildren<'a>>>,
        >,
        impl FnMut(NodeRef) -> Option<TokenDecl> + 'a,
    > {
        cst.children(self.syntax)
            .filter_map(|c| cst.get_rule(c, Rule::TokenList).map(|l| cst.children(l)))
            .flatten()
            .filter_map(|c| TokenDecl::cast(cst, c))
    }
    pub fn rule_decls<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<RuleDecl> + 'a> {
        cst.child_node_iter(self.syntax)
    }
    pub fn start_decls<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<StartDecl> + 'a> {
        cst.child_node_iter(self.syntax)
    }
    pub fn right_decls<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<RightDecl> + 'a> {
        cst.child_node_iter(self.syntax)
    }
    pub fn skip_decls<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<SkipDecl> + 'a> {
        cst.child_node_iter(self.syntax)
    }
}
impl Named for TokenDecl {
    fn name<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl TokenDecl {
    pub fn symbol<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Str)
    }
}
impl Named for RuleDecl {
    fn name<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl RuleDecl {
    pub fn regex(&self, cst: &Cst) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl StartDecl {
    pub fn rule_name<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl RightDecl {
    pub fn token_names<'a, F: FnMut((&'a str, Span))>(&self, cst: &'a Cst, f: F) {
        cst.children(self.syntax)
            .filter_map(|c| {
                cst.get_token(c, Token::Id)
                    .or_else(|| cst.get_token(c, Token::Str))
            })
            .for_each(f);
    }
}
impl SkipDecl {
    pub fn token_names<'a, F: FnMut((&'a str, Span))>(&self, cst: &'a Cst, f: F) {
        cst.children(self.syntax)
            .filter_map(|c| {
                cst.get_token(c, Token::Id)
                    .or_else(|| cst.get_token(c, Token::Str))
            })
            .for_each(f);
    }
}
impl Alternation {
    pub fn operands<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<Regex> + 'a> {
        cst.child_node_iter(self.syntax)
    }
}
impl Concat {
    pub fn operands<'a>(
        &self,
        cst: &'a Cst,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<Regex> + 'a> {
        cst.child_node_iter(self.syntax)
    }
}
impl Paren {
    pub fn inner(&self, cst: &Cst) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Optional {
    pub fn operand(&self, cst: &Cst) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Star {
    pub fn operand(&self, cst: &Cst) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Plus {
    pub fn operand(&self, cst: &Cst) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Name {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl Symbol {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Str)
    }
}
impl Predicate {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Predicate)
    }
}
impl Action {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Action)
    }
}
impl Binding {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Binding)
    }
}
impl OpenNode {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::OpenNode)
    }
    pub fn number<'a>(&self, cst: &'a Cst) -> Option<&'a str> {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('<'))
            .map(|(_, r)| r)
    }
}
impl CloseNode {
    pub fn value<'a>(&self, cst: &'a Cst) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::CloseNode)
    }
    pub fn number<'a>(&self, cst: &'a Cst) -> Option<&'a str> {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('>'))
            .map(|(l, _)| l)
    }
    pub fn node_name<'a>(&self, cst: &'a Cst) -> Option<&'a str> {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('>'))
            .map(|(_, r)| r)
    }
}
