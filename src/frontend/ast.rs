use crate::frontend::lexer::Token;
use crate::{Cst, CstChildren, Node, NodeRef, Rule, Span};

pub trait AstNode {
    fn cast(cst: &Cst<'_>, syntax: NodeRef) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> NodeRef;

    fn span(&self, cst: &Cst<'_>) -> Span {
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
            fn cast(cst: &Cst<'_>, syntax: NodeRef) -> Option<Self> {
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
            fn cast(cst: &Cst<'_>, syntax: NodeRef) -> Option<Self> {
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
ast_node!(PartDecl);
ast_node!(
    Regex,
    (
        OrderedChoice,
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
        Assertion,
        NodeRename,
        NodeElision,
        NodeMarker,
        NodeCreation,
        Commit,
        Return
    )
);
ast_node!(OrderedChoice);
ast_node!(Alternation);
ast_node!(Concat);
ast_node!(Paren);
ast_node!(Optional);
ast_node!(Star);
ast_node!(Plus);
ast_node!(Name);
ast_node!(Symbol);
ast_node!(Predicate);
ast_node!(Action);
ast_node!(Assertion);
ast_node!(NodeRename);
ast_node!(NodeElision);
ast_node!(NodeMarker);
ast_node!(NodeCreation);
ast_node!(Commit);
ast_node!(Return);

impl Cst<'_> {
    fn child_node<T: AstNode>(&self, syntax: NodeRef) -> Option<T> {
        self.children(syntax).find_map(|c| T::cast(self, c))
    }
    fn child_node_iter<T: AstNode>(
        &self,
        syntax: NodeRef,
    ) -> std::iter::FilterMap<CstChildren<'_>, impl FnMut(NodeRef) -> Option<T> + '_> {
        self.children(syntax).filter_map(|c| T::cast(self, c))
    }
    fn child_token(&self, syntax: NodeRef, token: Token) -> Option<(&str, Span)> {
        self.children(syntax)
            .find_map(|c| self.match_token(c, token))
    }
}

pub trait Named: AstNode {
    fn name<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)>;
}
impl File {
    #[allow(clippy::type_complexity, clippy::filter_map_bool_then)]
    pub fn token_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<
        std::iter::Flatten<
            std::iter::FilterMap<
                CstChildren<'a>,
                impl FnMut(NodeRef) -> Option<CstChildren<'a>> + use<'a>,
            >,
        >,
        impl FnMut(NodeRef) -> Option<TokenDecl> + 'a + use<'a>,
    > {
        cst.children(self.syntax)
            .filter_map(|c| cst.match_rule(c, Rule::TokenList).then(|| cst.children(c)))
            .flatten()
            .filter_map(|c| TokenDecl::cast(cst, c))
    }
    pub fn rule_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<RuleDecl> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
    pub fn start_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<
        CstChildren<'a>,
        impl FnMut(NodeRef) -> Option<StartDecl> + 'a + use<'a>,
    > {
        cst.child_node_iter(self.syntax)
    }
    pub fn right_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<
        CstChildren<'a>,
        impl FnMut(NodeRef) -> Option<RightDecl> + 'a + use<'a>,
    > {
        cst.child_node_iter(self.syntax)
    }
    pub fn skip_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<SkipDecl> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
    pub fn part_decls<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<PartDecl> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
}
impl Named for TokenDecl {
    fn name<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl TokenDecl {
    pub fn symbol<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Str)
    }
}
impl Named for RuleDecl {
    fn name<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl RuleDecl {
    pub fn regex(&self, cst: &Cst<'_>) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
    pub fn elision<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Hat)
    }
    pub fn is_elided(&self, cst: &Cst<'_>) -> bool {
        self.elision(cst).is_some()
    }
}
impl StartDecl {
    pub fn rule_name<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl RightDecl {
    pub fn token_names<'a, F: FnMut((&'a str, Span))>(&self, cst: &'a Cst<'_>, f: F) {
        cst.children(self.syntax)
            .filter_map(|c| {
                cst.match_token(c, Token::Id)
                    .or_else(|| cst.match_token(c, Token::Str))
            })
            .for_each(f);
    }
}
impl SkipDecl {
    pub fn token_names<'a, F: FnMut((&'a str, Span))>(&self, cst: &'a Cst<'_>, f: F) {
        cst.children(self.syntax)
            .filter_map(|c| {
                cst.match_token(c, Token::Id)
                    .or_else(|| cst.match_token(c, Token::Str))
            })
            .for_each(f);
    }
}
impl PartDecl {
    pub fn rule_names<'a, F: FnMut((&'a str, Span))>(&self, cst: &'a Cst<'_>, f: F) {
        cst.children(self.syntax)
            .filter_map(|c| cst.match_token(c, Token::Id))
            .for_each(f);
    }
}
impl OrderedChoice {
    pub fn operands<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<Regex> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
}
impl Alternation {
    pub fn operands<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<Regex> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
}
impl Concat {
    pub fn operands<'a>(
        &self,
        cst: &'a Cst<'_>,
    ) -> std::iter::FilterMap<CstChildren<'a>, impl FnMut(NodeRef) -> Option<Regex> + 'a + use<'a>>
    {
        cst.child_node_iter(self.syntax)
    }
}
impl Paren {
    pub fn inner(&self, cst: &Cst<'_>) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Optional {
    pub fn operand(&self, cst: &Cst<'_>) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Star {
    pub fn operand(&self, cst: &Cst<'_>) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Plus {
    pub fn operand(&self, cst: &Cst<'_>) -> Option<Regex> {
        cst.child_node(self.syntax)
    }
}
impl Name {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Id)
    }
}
impl Symbol {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Str)
    }
}
impl Predicate {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Predicate)
    }
    pub fn is_true(&self, cst: &Cst<'_>) -> bool {
        self.value(cst)
            .is_some_and(|(val, _)| matches!(&val[1..], "t"))
    }
}
impl Action {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Action)
    }
}
impl Assertion {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::Assertion)
    }
}
impl NodeRename {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::NodeRename)
    }
}
impl NodeMarker {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::NodeMarker)
    }
    pub fn number<'a>(&self, cst: &'a Cst<'_>) -> &'a str {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('<'))
            .map(|(_, r)| r)
            .unwrap()
    }
}
impl NodeCreation {
    pub fn value<'a>(&self, cst: &'a Cst<'_>) -> Option<(&'a str, Span)> {
        cst.child_token(self.syntax, Token::NodeCreation)
    }
    pub fn number<'a>(&self, cst: &'a Cst<'_>) -> Option<&'a str> {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('>'))
            .and_then(|(l, _)| (!l.is_empty()).then_some(l))
    }
    pub fn node_name<'a>(&self, cst: &'a Cst<'_>) -> Option<&'a str> {
        self.value(cst)
            .and_then(|(s, _)| s.split_once('>'))
            .and_then(|(_, r)| (!r.is_empty()).then_some(r))
    }
    pub fn whole_rule(&self, cst: &Cst<'_>) -> bool {
        self.number(cst).is_none()
    }
}
