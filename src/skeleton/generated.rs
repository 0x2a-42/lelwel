macro_rules! syntax_error_message {{
    [] => {{
        "invalid syntax".to_string()
    }};
    [$($tk:literal),+] => {{
        {{
            let expected = [$($tk),*];
            let mut msg = "invalid syntax, expected".to_string();
            if expected.len() > 1 {{
                msg.push_str(" one of: ");
            }} else {{
                msg.push_str(": ");
            }}
            let mut count = 0;
            for e in expected {{
                count += 1;
                let s = format!("{{}}", e);
                let s = if s.starts_with('<') && s.ends_with('>') && s.len() > 2 {{
                    s
                }} else {{
                    format!("'{{}}'", s)
                }};
                msg.push_str(&s);
                if count < expected.len() {{
                    msg.push_str(", ");
                }}
            }}
            msg
        }}
    }}
}}
macro_rules! err {{
    [$self:expr, $($tk:literal),*] => {{
        $self.create_diagnostic($self.span(), syntax_error_message!($($tk),*))
    }}
}}

#[derive(Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Rule {{
    Error,{0}
}}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct NodeRef(pub usize);

impl NodeRef {{
    #[allow(dead_code)]
    pub const ROOT: NodeRef = NodeRef(0);
}}

#[cfg(target_pointer_width = "64")]
#[derive(Debug, Copy, Clone)]
pub struct CstIndex([u8; 6]);

#[cfg(any(target_pointer_width = "16", target_pointer_width = "32"))]
#[derive(Debug, Copy, Clone)]
pub struct CstIndex(usize);

impl From<CstIndex> for usize {{
    #[cfg(target_pointer_width = "64")]
    #[inline]
    fn from(value: CstIndex) -> Self {{
        let [b0, b1, b2, b3, b4, b5] = value.0;
        usize::from_le_bytes([b0, b1, b2, b3, b4, b5, 0, 0])
    }}
    #[cfg(any(target_pointer_width = "16", target_pointer_width = "32"))]
    #[inline]
    fn from(value: CstIndex) -> Self {{
        value.0
    }}
}}
impl From<usize> for CstIndex {{
    #[cfg(target_pointer_width = "64")]
    #[inline]
    fn from(value: usize) -> Self {{
        let [b0, b1, b2, b3, b4, b5, b6, b7] = value.to_le_bytes();
        debug_assert!(b6 == 0 && b7 == 0);
        Self([b0, b1, b2, b3, b4, b5])
    }}
    #[cfg(any(target_pointer_width = "16", target_pointer_width = "32"))]
    #[inline]
    fn from(value: usize) -> Self {{
        Self(value)
    }}
}}

/// Type of a node in the CST.
///
/// The nodes for rules contain the offset to their last child node.
/// The nodes for tokens contain an index to their span.
///
/// On 64 bit platforms offsets and indices are stored as 48 bit integers.
/// This allows the `Node` type to be 8 bytes in size as long as the `Rule`
/// and `Token` enums are one byte in size.
#[derive(Debug, Copy, Clone)]
pub enum Node {{
    Rule(Rule, CstIndex),
    Token(Token, CstIndex),
}}

#[derive(Clone, Copy)]
struct MarkOpened(usize);
#[derive(Clone, Copy)]
struct MarkClosed(usize);
#[derive(Clone)]
struct MarkTruncation {{
    node_count: usize,
    token_count: usize,
}}

/// An iterator for child nodes of a CST node.
pub struct CstChildren<'a> {{
    iter: std::slice::Iter<'a, Node>,
    offset: usize,
}}
impl Iterator for CstChildren<'_> {{
    type Item = NodeRef;

    fn next(&mut self) -> Option<Self::Item> {{
        let offset = self.offset;
        self.offset += 1;
        if let Some(node) = self.iter.next() {{
            if let Node::Rule(_, end_offset) = node {{
                let end_offset = usize::from(*end_offset);
                if end_offset > 0 {{
                    self.iter.nth(end_offset.saturating_sub(1));
                    self.offset += end_offset;
                }}
            }}
            Some(NodeRef(offset))
        }} else {{
            None
        }}
    }}
}}

pub type Span = core::ops::Range<usize>;

/// A concrete syntax tree (CST) type.
///
/// Nodes are layed out linearly in memory.
/// Spans for tokens are directly stored in the `spans` vector.
/// Spans for rule nodes are calculated based on their contained token nodes.
///
/// # Example
/// This syntax tree
/// ```text
/// foo
///   bar
///     A
///     b
///   C
/// ```
/// will have the following `nodes` vector.
/// ```text
/// [
///    Node::Rule(Rule::Foo, 4),
///    Node::Rule(Rule::Bar, 2),
///    Node::Token(Token::A, 0),
///    Node::Token(Token::B, 1),
///    Node::Token(Token::C, 2),
/// ]
/// ```
pub struct Cst<'a> {{
    source: &'a str,
    spans: Vec<Span>,
    nodes: Vec<Node>,
    token_count: usize,
}}
#[allow(dead_code)]
impl<'a> Cst<'a> {{
    fn new(source: &'a str, spans: Vec<Span>) -> Self {{
        let nodes = Vec::with_capacity(spans.len() * 2);
        Self {{
            source,
            spans,
            nodes,
            token_count: 0,
        }}
    }}
    fn open(&mut self) -> MarkOpened {{
        let mark = MarkOpened(self.nodes.len());
        self.nodes.push(Node::Rule(Rule::Error, 0.into()));
        mark
    }}
    fn close(&mut self, mark: MarkOpened, rule: Rule) -> MarkClosed {{
        self.nodes[mark.0] = Node::Rule(rule, (self.nodes.len() - 1 - mark.0).into());
        MarkClosed(mark.0)
    }}
    fn advance(&mut self, token: Token) {{
        self.nodes.push(Node::Token(token, self.token_count.into()));
        self.token_count += 1;
    }}
    fn open_before(&mut self, mark: MarkClosed) -> MarkOpened {{
        self.nodes.insert(mark.0, Node::Rule(Rule::Error, 0.into()));
        MarkOpened(mark.0)
    }}
    fn mark(&self) -> MarkClosed {{
        MarkClosed(self.nodes.len())
    }}
    fn mark_truncation(&self) -> MarkTruncation {{
        MarkTruncation {{
            node_count: self.nodes.len(),
            token_count: self.token_count,
        }}
    }}
    fn truncate(&mut self, mark: MarkTruncation) {{
        self.nodes.truncate(mark.node_count);
        self.token_count = mark.token_count;
    }}
    /// Returns an iterator over the children of the node referenced by `node_ref`.
    pub fn children(&self, node_ref: NodeRef) -> CstChildren {{
        let iter = if let Node::Rule(_, end_offset) = self.nodes[node_ref.0] {{
            self.nodes[node_ref.0 + 1..node_ref.0 + usize::from(end_offset) + 1].iter()
        }} else {{
            std::slice::Iter::default()
        }};
        CstChildren {{
            iter,
            offset: node_ref.0 + 1,
        }}
    }}
    /// Returns the the node referenced by `node_ref`.
    pub fn get(&self, node_ref: NodeRef) -> Node {{
        self.nodes[node_ref.0]
    }}
    /// Returns the span for the node referenced by `node_ref`.
    ///
    /// For rules the span is calculated based on the first and last non-skipped token.
    /// If there are only skipped tokens the function returns `None`.
    pub fn get_span(&self, node_ref: NodeRef) -> Option<Span> {{
        fn find_non_skip<'a>(mut iter: impl Iterator<Item = &'a Node>) -> Option<usize> {{
            iter.find_map(|node| match node {{
                Node::Token(
                    Token::Error{1},
                    _,
                )
                | Node::Rule(..) => None,
                Node::Token(_, idx) => Some(usize::from(*idx)),
            }})
        }}
        match self.nodes[node_ref.0] {{
            Node::Token(_, idx) => Some(self.spans[usize::from(idx)].clone()),
            Node::Rule(_, end_offset) => {{
                let end = node_ref.0 + usize::from(end_offset);
                let first = find_non_skip(self.nodes[node_ref.0 + 1..=end].iter());
                let last = find_non_skip(self.nodes[node_ref.0 + 1..=end].iter().rev());
                if let (Some(first), Some(last)) = (first, last) {{
                    Some(self.spans[first].start..self.spans[last].end)
                }} else {{
                    None
                }}
            }}
        }}
    }}
    /// Returns the slice and span of the node referenced by `node_ref` if it matches `matched_token`.
    pub fn match_token(&self, node_ref: NodeRef, matched_token: Token) -> Option<(&'a str, Span)> {{
        match self.nodes[node_ref.0] {{
            Node::Token(token, idx) if token == matched_token => {{
                let span = &self.spans[usize::from(idx)];
                Some((&self.source[span.clone()], span.clone()))
            }}
            _ => None,
        }}
    }}
    /// Checks if the node referenced by `node_ref` matches `matched_rule`.
    pub fn match_rule(&self, node_ref: NodeRef, matched_rule: Rule) -> bool {{
        matches!(self.nodes[node_ref.0], Node::Rule(rule, _) if rule == matched_rule)
    }}
}}

impl std::fmt::Display for Cst<'_> {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        const DEPTH: &str = "    ";
        fn rec(
            cst: &Cst,
            f: &mut std::fmt::Formatter<'_>,
            node_ref: NodeRef,
            indent: usize,
        ) -> std::fmt::Result {{
            match cst.get(node_ref) {{
                Node::Rule(rule, _) => {{
                    if let Some(span) = cst.get_span(node_ref) {{
                        writeln!(f, "{{}}{{rule:?}} [{{span:?}}]", DEPTH.repeat(indent),)?;
                    }} else {{
                        writeln!(f, "{{}}{{rule:?}}", DEPTH.repeat(indent),)?;
                    }}
                    for child_node_ref in cst.children(node_ref) {{
                        rec(cst, f, child_node_ref, indent + 1)?;
                    }}
                    Ok(())
                }}
                Node::Token(token, idx) => {{
                    let span = &cst.spans[usize::from(idx)];
                    writeln!(
                        f,
                        "{{}}{{:?}} {{:?}} [{{:?}}]",
                        DEPTH.repeat(indent),
                        token,
                        &cst.source[span.clone()],
                        span,
                    )
                }}
            }}
        }}
        rec(self, f, NodeRef::ROOT, 0)
    }}
}}
impl std::fmt::Debug for Rule {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        match self {{
            Rule::Error => write!(f, "error"),{3}
        }}
    }}
}}

macro_rules! expect {{
    ($token:ident, $sym:literal, $self:expr, $diags:expr) => {{
        if let Token::$token = $self.current {{
            $self.advance(false);
        }} else {{
            $self.error($diags, err![$self, $sym]);
        }}
    }};
}}
#[allow(unused_macros)]
macro_rules! try_expect {{
    ($token:ident, $sym:literal, $self:expr, $diags:expr) => {{
        if let Token::$token = $self.current {{
            $self.advance(false);
        }} else {{
            if $self.in_ordered_choice {{
                return None;
            }}
            $self.error($diags, err![$self, $sym]);
        }}
    }};
}}

struct ParserState {{
    pos: usize,
    current: Token,
    truncation_mark: MarkTruncation,
}}
pub struct Parser<'a> {{
    cst: Cst<'a>,
    tokens: Vec<Token>,
    pos: usize,
    current: Token,
    last_error_span: Span,
    max_offset: usize,
    #[allow(dead_code)]
    context: Context<'a>,
    error_cooldown: bool,
    #[allow(dead_code)]
    in_ordered_choice: bool,
}}
#[allow(clippy::while_let_loop, dead_code, unused_parens)]
impl<'a> Parser<'a> {{
    fn error(&mut self, diags: &mut Vec<Diagnostic>, diag: Diagnostic) {{
        if self.error_cooldown || self.last_error_span == self.span() {{
            return;
        }}
        self.last_error_span = self.span();
        diags.push(diag);
    }}
    fn advance(&mut self, error: bool) {{
        if !error {{
            self.error_cooldown = false;
        }}
        self.cst.advance(self.current);
        loop {{
            self.pos += 1;
            match self.tokens.get(self.pos) {{
                Some(token @ (Token::Error{1})) => {{
                    self.cst.advance(*token);
                    continue;
                }}
                Some(token) => {{
                    self.current = *token;
                    break;
                }}
                None => {{
                    self.current = Token::EOF;
                    break;
                }}
            }}
        }}
    }}
    fn is_skipped(token: Token) -> bool {{
        matches!(token, Token::Error{1})
    }}
    fn init_skip(&mut self) {{
        loop {{
            match self.tokens.get(self.pos) {{
                Some(token @ (Token::Error{1})) => {{
                    self.pos += 1;
                    self.cst.advance(*token);
                    continue;
                }}
                Some(token) => {{
                    self.current = *token;
                    break;
                }}
                None => {{
                    self.current = Token::EOF;
                    break;
                }}
            }}
        }}
    }}
    fn advance_with_error(&mut self, diags: &mut Vec<Diagnostic>, diag: Diagnostic) {{
        let m = self.cst.open();
        self.error(diags, diag);
        self.error_cooldown = true;
        self.advance(true);
        self.close(m, Rule::Error, diags);
    }}
    fn peek(&self, lookahead: usize) -> Token {{
        self.tokens
            .iter()
            .skip(self.pos)
            .filter(|token| !Self::is_skipped(**token))
            .nth(lookahead)
            .map_or(Token::EOF, |it| *it)
    }}
    fn peek_left(&self, lookbehind: usize) -> Token {{
        self.tokens
            .iter()
            .take(self.pos + 1)
            .rev()
            .filter(|token| !Self::is_skipped(**token))
            .nth(lookbehind)
            .map_or(Token::EOF, |it| *it)
    }}
    fn span(&self) -> Span {{
        self.cst.spans
            .get(self.pos)
            .map_or(self.max_offset..self.max_offset, |span| span.clone())
    }}
    fn close(&mut self, mark: MarkOpened, rule: Rule, diags: &mut Vec<Diagnostic>) -> MarkClosed {{
        let m = self.cst.close(mark, rule);
        self.create_node(rule, NodeRef(m.0), diags);
        m
    }}
    fn get_state(&self) -> ParserState {{
        ParserState {{
            pos: self.pos,
            current: self.current,
            truncation_mark: self.cst.mark_truncation(),
        }}
    }}
    fn set_state(&mut self, state: &ParserState) {{
        self.pos = state.pos;
        self.current = state.current;
        self.cst.truncate(state.truncation_mark.clone());
    }}
    /// Returns the CST for a parse with the given `source` file and writes diagnostics to `diags`.
    ///
    /// The context can be explicitly defined for the parse.
    pub fn parse_with_context(
        source: &'a str,
        diags: &mut Vec<Diagnostic>,
        context: Context<'a>,
    ) -> Cst<'a> {{
        let (tokens, spans) = Self::create_tokens(source, diags);
        let max_offset = source.len();
        let mut parser = Self {{
            current: Token::EOF,
            cst: Cst::new(source, spans),
            tokens,
            pos: 0,
            last_error_span: Span::default(),
            max_offset,
            context,
            error_cooldown: false,
            in_ordered_choice: false,
        }};
        parser.rule_{2}(diags);
        parser.cst
    }}
    /// Returns the CST for a parse with the given `source` file and writes diagnostics to `diags`.
    ///
    /// The context will be default initialized for the parse.
    pub fn parse(
        source: &'a str,
        diags: &mut Vec<Diagnostic>,
    ) -> Cst<'a> {{
        Self::parse_with_context(source, diags, Context::default())
    }}
