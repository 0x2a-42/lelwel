use crate::ast::*;
use crate::lexer::{tokenize, Token};
use codespan_reporting::diagnostic::Label;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

struct Scope<'a> {
    declared_names: std::collections::HashMap<&'a str, bool>,
    allows_typedef: bool,
}
impl Scope<'_> {
    fn new(allows_typedef: bool) -> Self {
        Self {
            declared_names: Default::default(),
            allows_typedef,
        }
    }
}

pub struct Context<'a> {
    scopes: Vec<Scope<'a>>,
    last_parameter_scope: Scope<'a>,
    has_type_specifier: bool,
    in_typedef: Vec<Option<Span>>,
    last_seen_declarator: Option<Declarator>,
    first_declarator_in_list: Option<Declarator>,
}

impl Default for Context<'_> {
    fn default() -> Self {
        let global_scope = Scope {
            declared_names: std::collections::HashMap::from_iter([
                ("__builtin_va_list", true),
                ("_Float16", true),
                ("__fp16", true),
                ("__bf16", true),
                ("_Float32", true),
                ("_Float64", true),
                ("_Float128", true),
                ("_Float32x", true),
                ("_Float64x", true),
                ("_Float128x", true),
                ("__SIZE_TYPE__", true),
                ("__label__", true),
            ]),
            allows_typedef: true,
        };
        Self {
            scopes: vec![global_scope],
            last_parameter_scope: Scope::new(false),
            has_type_specifier: false,
            in_typedef: vec![],
            last_seen_declarator: None,
            first_declarator_in_list: None,
        }
    }
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl Parser<'_> {
    fn check_missing_type_specifier(
        &self,
        decl_specs: Option<DeclarationSpecifiers>,
        decl: Option<Declarator>,
        diags: &mut Vec<Diagnostic>,
    ) {
        let has_type_specifier = decl_specs
            .map(|decl_specs| decl_specs.has_type_specifier(&self.cst))
            .unwrap_or(false);
        if !has_type_specifier {
            let span = decl
                .and_then(|decl| decl.name(&self.cst))
                .map(|(_, span)| span)
                .unwrap_or_else(|| self.span());
            diags.push(
                Diagnostic::warning()
                    .with_message(
                        "type specifier missing, ISO C99 and later do not support implicit int",
                    )
                    .with_labels(vec![Label::primary((), span)]),
            );
        }
    }

    fn is_type_name(&self, pos: usize) -> bool {
        let name = &self.cst.source[self.cst.spans[pos].clone()];
        for scopes in self.context.scopes.iter().rev() {
            if let Some(is_type) = scopes.declared_names.get(name) {
                return *is_type;
            }
        }
        false
    }

    fn is_parenthesized_type(&self) -> bool {
        let lookahead = self.peek(1);
        if matches!(
            lookahead,
            Token::Alignas
                | Token::Atomic
                | Token::Attribute
                | Token::Bool
                | Token::Char
                | Token::Complex
                | Token::Const
                | Token::Double
                | Token::Enum
                | Token::Float
                | Token::Imaginary
                | Token::Int
                | Token::Int128
                | Token::Long
                | Token::Restrict
                | Token::Short
                | Token::Signed
                | Token::Struct
                | Token::TypeOf
                | Token::Union
                | Token::Unsigned
                | Token::Void
                | Token::Volatile
        ) {
            return true;
        }
        if lookahead == Token::Identifier {
            self.tokens[self.pos..]
                .iter()
                .enumerate()
                .filter_map(|(i, tok)| {
                    if !Self::is_skipped(*tok) {
                        Some(i)
                    } else {
                        None
                    }
                })
                .nth(1)
                .map_or(false, |i| self.is_type_name(self.pos + i))
        } else {
            false
        }
    }
}

#[allow(clippy::ptr_arg)]
impl ParserCallbacks for Parser<'_> {
    fn create_tokens(source: &str, diags: &mut Vec<Diagnostic>) -> (Vec<Token>, Vec<Span>) {
        tokenize(source, diags)
    }
    fn create_diagnostic(&self, span: Span, message: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(message)
            .with_labels(vec![Label::primary((), span)])
    }
    fn create_node(&mut self, rule: Rule, node: NodeRef, diags: &mut Vec<Diagnostic>) {
        match rule {
            Rule::Declaration => {
                let decl = Declaration::cast(&self.cst, node).unwrap();
                if let Some(init_decl_list) = decl.init_declarator_list(&self.cst) {
                    for init_decl in init_decl_list.init_declarators(&self.cst) {
                        self.check_missing_type_specifier(
                            decl.declaration_specifiers(&self.cst),
                            init_decl.declarator(&self.cst),
                            diags,
                        );
                    }
                }
            }
            Rule::FunctionDefinition => {
                let def = FunctionDefinition::cast(&self.cst, node).unwrap();
                self.check_missing_type_specifier(
                    def.declaration_specifiers(&self.cst),
                    def.declarator(&self.cst),
                    diags,
                );
            }
            Rule::Declarator => {
                let decl = Declarator::cast(&self.cst, node);
                if let Some(decl) = decl {
                    let is_type = self.context.in_typedef.last().unwrap().is_some();
                    if let Some((name, name_span)) = decl.name(&self.cst) {
                        if let Some(was_type) = self
                            .context
                            .scopes
                            .last_mut()
                            .unwrap()
                            .declared_names
                            .insert(name, is_type)
                        {
                            if was_type != is_type {
                                diags.push(
                                    Diagnostic::error()
                                        .with_message("redeclaration as different kind of symbol")
                                        .with_labels(vec![Label::primary((), name_span)]),
                                );
                            }
                        }
                    }
                    let direct_decl = decl.direct_declarator(&self.cst);
                    if !matches!(direct_decl, Some(DirectDeclarator::ParenDeclarator(_))) {
                        self.context.last_seen_declarator = Some(decl);
                    }
                }
            }
            Rule::Enumerator => {
                if let Some((name, name_span)) =
                    Enumerator::cast(&self.cst, node).unwrap().name(&self.cst)
                {
                    if let Some(was_type) = self
                        .context
                        .scopes
                        .last_mut()
                        .unwrap()
                        .declared_names
                        .insert(name, false)
                    {
                        if was_type {
                            diags.push(
                                Diagnostic::error()
                                    .with_message("redeclaration as different kind of symbol")
                                    .with_labels(vec![Label::primary((), name_span)]),
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }

    fn predicate_postfix_expr_1(&self) -> bool {
        self.is_parenthesized_type()
    }
    fn predicate_unary_expr_1(&self) -> bool {
        if self.is_parenthesized_type() {
            // check that this is not a compound literal
            let mut it = self.tokens[self.pos..]
                .iter()
                .filter(|tok| !Self::is_skipped(**tok))
                .skip(1);
            let mut paren_depth = 1;
            for tok in it.by_ref() {
                match tok {
                    Token::LPar => paren_depth += 1,
                    Token::RPar => paren_depth -= 1,
                    _ => {}
                }
                if paren_depth == 0 {
                    break;
                }
            }
            !matches!(it.next(), Some(Token::LBrace))
        } else {
            false
        }
    }
    fn predicate_cast_expr_1(&self) -> bool {
        if self.is_parenthesized_type() {
            // check this is not a compound literal
            let mut it = self.tokens[self.pos..].iter().skip(1);
            let mut paren_depth = 1;
            loop {
                match it.next() {
                    Some(Token::LBrace) if paren_depth == 0 => return false,
                    Some(Token::LPar) if paren_depth != 0 => paren_depth += 1,
                    Some(Token::RPar) => paren_depth -= 1,
                    Some(tok) if paren_depth > 0 || Self::is_skipped(*tok) => {}
                    _ => return true,
                }
            }
        } else {
            false
        }
    }
    fn predicate_declaration_specifiers_1(&self) -> bool {
        self.current != Token::Identifier
            || (!self.context.has_type_specifier && self.is_type_name(self.pos))
    }
    fn predicate_declaration_specifiers_2(&self) -> bool {
        if self.current == Token::Atomic {
            self.peek(1) == Token::LPar
        } else {
            true
        }
    }
    fn predicate_specifier_qualifier_list_1(&self) -> bool {
        self.current != Token::Identifier || self.is_type_name(self.pos)
    }
    fn predicate_specifier_qualifier_list_2(&self) -> bool {
        if self.current == Token::Atomic {
            self.peek(1) == Token::LPar
        } else {
            true
        }
    }
    fn predicate_enumerator_list_1(&self) -> bool {
        // use extra lookahead
        self.peek(1) != Token::RBrace
    }
    fn predicate_alignment_specifier_1(&self) -> bool {
        self.current != Token::Identifier || self.is_type_name(self.pos)
    }
    fn predicate_direct_declarator_1(&self) -> bool {
        // use extra lookahead
        !self.is_type_name(self.pos)
            && (self.peek(1) == Token::Comma || self.peek(1) == Token::RPar)
    }
    fn predicate_direct_declarator_2(&self) -> bool {
        // use extra lookahead
        self.peek(1) == Token::RBrak
    }
    fn predicate_parameter_list_1(&self) -> bool {
        // use extra lookahead
        self.peek(1) != Token::RPar && self.peek(1) != Token::Ellipsis
    }
    fn predicate_parameter_declaration_1(&self) -> bool {
        // check if the parameter declaration has a direct or abstract declarator
        // if all initial '*', type qualifier, and '(' tokens are followed by an identifier it must be
        // a normal declarator
        if self.current != Token::Star && self.current != Token::LPar {
            return true;
        }
        let mut it = self.tokens[self.pos..]
            .iter()
            .filter(|tok| !Self::is_skipped(**tok));
        while let Some(tok) = it.next() {
            match tok {
                Token::Attribute => {
                    if !matches!(it.next(), Some(Token::LPar)) {
                        continue;
                    }
                    let mut paren_depth = 1;
                    for tok in it.by_ref() {
                        match tok {
                            Token::LPar => paren_depth += 1,
                            Token::RPar => paren_depth -= 1,
                            _ => {}
                        }
                        if paren_depth == 0 {
                            break;
                        }
                    }
                }
                Token::LPar
                | Token::Star
                | Token::Const
                | Token::Volatile
                | Token::Restrict
                | Token::Atomic => {}
                _ => return *tok == Token::Identifier,
            }
        }
        false
    }
    fn predicate_direct_abstract_declarator_1(&self) -> bool {
        // use extra lookahead
        self.peek(1) == Token::RBrak
    }
    fn predicate_initializer_list_1(&self) -> bool {
        // use extra lookahead
        self.peek(1) != Token::RBrace
    }
    fn predicate_statement_1(&self) -> bool {
        // use extra lookahead
        self.current != Token::Identifier || self.peek(1) == Token::Colon
    }
    fn predicate_block_item_1(&self) -> bool {
        match self.current {
            Token::Identifier => self.is_type_name(self.pos),
            Token::Asm | Token::Extension => false,
            _ => true,
        }
    }
    fn predicate_for_statement_1(&self) -> bool {
        self.current != Token::Identifier || self.is_type_name(self.pos)
    }
    fn predicate_external_declaration_1(&self) -> bool {
        self.current != Token::Identifier || self.is_type_name(self.pos)
    }
    fn predicate_external_declaration_2(&self) -> bool {
        if let Some(decl) = self.context.first_declarator_in_list {
            let direct_decl = decl.direct_declarator(&self.cst);
            if let Some(DirectDeclarator::FunctionDeclarator(func_decl)) = direct_decl {
                func_decl.is_complete(&self.cst)
            } else {
                false
            }
        } else {
            false
        }
    }
    fn predicate_attrib_1(&self) -> bool {
        // use extra lookahead
        self.peek(1) == Token::Comma || self.peek(1) == Token::RPar
    }
    fn predicate_typeof_specifier_1(&self) -> bool {
        self.current != Token::Identifier || self.is_type_name(self.pos)
    }

    fn action_compound_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_compound_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_function_body_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }

    // since C99 there is a block scope for selection and iteration statements
    fn action_if_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_if_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_switch_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_switch_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_while_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_while_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_do_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_do_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_for_statement_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(true));
    }
    fn action_for_statement_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }
    fn action_struct_or_union_specifier_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(false));
    }
    fn action_struct_or_union_specifier_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.pop();
    }

    fn action_declaration_specifiers_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.has_type_specifier = true;
    }
    fn action_declaration_specifiers_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.has_type_specifier = false;
    }

    fn action_storage_class_specifier_1(&mut self, diags: &mut Vec<Diagnostic>) {
        if !self.context.scopes.last().unwrap().allows_typedef {
            diags.push(
                Diagnostic::error()
                    .with_message("typedef not allowed in this scope")
                    .with_labels(vec![Label::primary((), self.span())]),
            );
            return;
        }
        *self.context.in_typedef.last_mut().unwrap() = Some(self.span());
    }
    fn action_declaration_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.push(None);
    }
    fn action_declaration_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.pop();
    }
    fn action_struct_declaration_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.push(None);
    }
    fn action_struct_declaration_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.pop();
    }
    fn action_direct_declarator_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.scopes.push(Scope::new(false));
    }
    fn action_direct_declarator_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.last_parameter_scope = self.context.scopes.pop().unwrap();
    }
    fn action_external_declaration_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.push(None);
    }
    fn action_external_declaration_2(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.first_declarator_in_list = self.context.last_seen_declarator;
    }
    fn action_external_declaration_3(&mut self, diags: &mut Vec<Diagnostic>) {
        if let Some(typedef_span) = self.context.in_typedef.last().unwrap() {
            if let Some((name, _name_span)) = self
                .context
                .first_declarator_in_list
                .and_then(|decl| decl.name(&self.cst))
            {
                diags.push(
                    Diagnostic::error()
                        .with_message("typedef in function definition")
                        .with_labels(vec![Label::primary((), typedef_span.clone())]),
                );
                self.context
                    .scopes
                    .last_mut()
                    .unwrap()
                    .declared_names
                    .remove(name);
            }
        }
        // use last parameter scope for compound statement of function definition
        self.context.scopes.push(std::mem::replace(
            &mut self.context.last_parameter_scope,
            Scope::new(false),
        ));
    }
    fn action_external_declaration_4(&mut self, _diags: &mut Vec<Diagnostic>) {
        // allow typedefs in function body
        self.context.scopes.last_mut().unwrap().allows_typedef = true;
    }
    fn action_external_declaration_5(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.context.in_typedef.pop();
        self.context.first_declarator_in_list = None;
    }
}
