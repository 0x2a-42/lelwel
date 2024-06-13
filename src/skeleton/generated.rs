#[allow(unused_macros)]
macro_rules! syntax_error_message {{
    [$span:expr, $($tk:literal),*] => {{
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Rule {{
    Error,{0}
}}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct NodeRef(pub CstIndex);

impl NodeRef {{
    #[allow(dead_code)]
    pub const ROOT: NodeRef = NodeRef(0);
}}

#[derive(Debug, Copy, Clone)]
pub enum Node {{
    Rule(Rule, CstIndex),
    Token(CstIndex),
}}

#[derive(Clone, Copy)]
struct MarkOpened(CstIndex);
#[derive(Clone, Copy)]
struct MarkClosed(CstIndex);

pub struct CstChildren<'a> {{
    iter: std::slice::Iter<'a, Node>,
    offset: CstIndex,
}}
#[allow(clippy::unnecessary_cast)]
impl<'a> Iterator for CstChildren<'a> {{
    type Item = NodeRef;

    fn next(&mut self) -> Option<Self::Item> {{
        let offset = self.offset;
        let node = self.iter.next();
        self.offset += 1;
        if let Some(Node::Rule(_, end_offset)) = node {{
            if *end_offset > 0 {{
                self.iter.nth((*end_offset as usize).saturating_sub(1));
                self.offset += *end_offset;
            }}
        }}
        if node.is_some() {{
            Some(NodeRef(offset))
        }} else {{
            None
        }}
    }}
}}

pub struct Cst<'a> {{
    source: &'a str,
    tokens: Vec<Token>,
    ranges: Vec<std::ops::Range<CstIndex>>,
    nodes: Vec<Node>,
    token_count: CstIndex,
}}
#[allow(clippy::unnecessary_cast, dead_code)]
impl<'a> Cst<'a> {{
    fn new(source: &'a str, tokens: Vec<Token>, ranges: Vec<std::ops::Range<CstIndex>>) -> Self {{
        Self {{
            source,
            tokens,
            ranges,
            nodes: vec![],
            token_count: 0,
        }}
    }}
    fn open(&mut self) -> MarkOpened {{
        let mark = MarkOpened(self.nodes.len() as CstIndex);
        self.nodes.push(Node::Rule(Rule::Error, 0));
        mark
    }}
    fn close(&mut self, mark: MarkOpened, rule: Rule) -> MarkClosed {{
        self.nodes[mark.0 as usize] = Node::Rule(rule, self.nodes.len() as CstIndex - 1 - mark.0);
        MarkClosed(mark.0)
    }}
    fn advance(&mut self) {{
        self.nodes.push(Node::Token(self.token_count));
        self.token_count += 1;
    }}
    fn open_before(&mut self, mark: MarkClosed) -> MarkOpened {{
        self.nodes
            .insert(mark.0 as usize, Node::Rule(Rule::Error, 0));
        MarkOpened(mark.0)
    }}
    fn mark(&self) -> MarkClosed {{
        MarkClosed(self.nodes.len() as CstIndex)
    }}
    pub fn children(&self, node: NodeRef) -> CstChildren {{
        let iter = if let Some(Node::Rule(_, end_offset)) = self.nodes.get(node.0 as usize) {{
            self.nodes[node.0 as usize + 1..node.0 as usize + *end_offset as usize + 1].iter()
        }} else {{
            std::slice::Iter::default()
        }};
        CstChildren {{
            iter,
            offset: node.0 + 1,
        }}
    }}
    pub fn get(&self, node: NodeRef) -> Node {{
        self.nodes[node.0 as usize]
    }}
    pub fn get_span(&self, node: NodeRef) -> Option<Span> {{
        match self.nodes.get(node.0 as usize) {{
            Some(Node::Token(idx)) => {{
               let range = &self.ranges[*idx as usize];
               Some(range.start as usize .. range.end as usize)
            }}
            Some(Node::Rule(_, _)) => self
                .children(node)
                .filter(|node_ref| {{
                    if let Node::Token(idx) = self.get(*node_ref) {{
                        !matches!(
                            self.tokens[idx as usize],
                            Token::Error{1}
                        )
                    }} else {{
                        true
                    }}
                }})
                .filter_map(|n| self.get_span(n))
                .reduce(|acc, e| acc.start.min(e.start) .. acc.end.max(e.end)),
            None => None,
        }}
    }}
    pub fn get_token(&self, node: NodeRef, token: Token) -> Option<(&'a str, Span)> {{
        if let Some(Node::Token(idx)) = self.nodes.get(node.0 as usize) {{
            let tok = self.tokens[*idx as usize];
            let range = &self.ranges[*idx as usize];
            if token == tok {{
                let span = range.start as usize..range.end as usize;
                Some((&self.source[span.clone()], span))
            }} else {{
                None
            }}
        }} else {{
            None
        }}
    }}
    pub fn get_rule(&self, node: NodeRef, rule: Rule) -> Option<NodeRef> {{
        if let Some(Node::Rule(r, _)) = self.nodes.get(node.0 as usize) {{
            if rule == *r {{
                Some(node)
            }} else {{
                None
            }}
        }} else {{
            None
        }}
    }}
}}

#[allow(clippy::unnecessary_cast)]
impl std::fmt::Display for Cst<'_> {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        const DEPTH: &str = "    ";
        fn rec(
            cst: &Cst,
            f: &mut std::fmt::Formatter<'_>,
            idx: NodeRef,
            indent: usize,
        ) -> std::fmt::Result {{
            match cst.get(idx) {{
                Node::Rule(rule, _) => {{
                    if let Some(range) = cst.get_span(idx) {{
                        writeln!(f, "{{}}{{rule:?}} [{{range:?}}]", DEPTH.repeat(indent),)?;
                    }} else {{
                        writeln!(f, "{{}}{{rule:?}}", DEPTH.repeat(indent),)?;
                    }}
                }}
                Node::Token(tok) => {{
                    let range = &cst.ranges[tok as usize];
                    writeln!(
                        f,
                        "{{}}{{:?}} {{:?}} [{{:?}}]",
                        DEPTH.repeat(indent),
                        cst.tokens[tok as usize],
                        &cst.source[range.start as usize..range.end as usize],
                        range,
                    )?;
                }}
            }}
            for c in cst.children(idx) {{
                rec(cst, f, c, indent + 1)?;
            }}
            Ok(())
        }}
        rec(self, f, NodeRef(0), 0)
    }}
}}

macro_rules! expect {{
    ($tok:ident, $sym:literal, $self:expr, $diags:expr) => {{
        if let Token::$tok = $self.current {{
            $self.advance(false);
        }} else {{
            $self.error($diags, err![$self.span(), $sym]);
        }}
    }};
}}

pub struct Parser<'a> {{
    cst: Cst<'a>,
    pos: usize,
    current: Token,
    error_cooldown: bool,
    max_offset: usize,
    #[allow(dead_code)]
    context: Context<'a>,
}}
#[allow(clippy::while_let_loop, dead_code)]
impl<'a> Parser<'a> {{
    fn error(&self, diags: &mut Vec<Diagnostic>, diag: Diagnostic) {{
        if self.error_cooldown {{
            return;
        }}
        if let Some(last) = diags.last() {{
            if last.labels.first().unwrap().range == diag.labels.first().unwrap().range {{
                return;
            }}
        }}
        diags.push(diag);
    }}
    fn advance(&mut self, error: bool) {{
        if !error {{
            self.error_cooldown = false;
        }}
        loop {{
            self.pos += 1;
            match self.cst.tokens.get(self.pos) {{
                Some(Token::Error{1}) => {{
                    self.cst.advance();
                    continue;
                }}
                Some(tok) => {{
                    self.current = *tok;
                    self.cst.advance();
                    break;
                }}
                None => {{
                    self.current = Token::EOF;
                    self.cst.advance();
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
            match self.cst.tokens.get(self.pos) {{
                Some(Token::Error{1}) => {{
                    self.pos += 1;
                    self.cst.advance();
                    continue;
                }}
                Some(tok) => {{
                    self.current = *tok;
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
    #[allow(dead_code)]
    fn peek(&self, lookahead: usize) -> Token {{
        self.cst
            .tokens
            .iter()
            .skip(self.pos)
            .filter(|token| !Self::is_skipped(**token))
            .nth(lookahead)
            .map_or(Token::EOF, |it| *it)
    }}
    fn span(&self) -> std::ops::Range<CstIndex> {{
        self.cst.ranges
            .get(self.pos)
            .map_or(self.max_offset..self.max_offset, |span| span.clone())
    }}
    fn close(&mut self, mark: MarkOpened, rule: Rule, diags: &mut Vec<Diagnostic>) -> MarkClosed {{
        let m = self.cst.close(mark, rule);
        self.build(rule, NodeRef(m.0), diags);
        m
    }}
    pub fn parse(
        source: &'a str,
        tokens: Vec<Token>,
        ranges: Vec<std::ops::Range<CstIndex>>,
        diags: &mut Vec<Diagnostic>,
    ) -> Cst<'a> {{
        let max_offset = source.len();
        let mut parser = Self {{
            current: Token::EOF,
            cst: Cst::new(source, tokens, ranges),
            pos: 0,
            error_cooldown: false,
            max_offset,
            context: Context::default(),
        }};
        parser.{2}(diags);
        parser.cst
    }}
