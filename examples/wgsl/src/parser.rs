use super::lexer::{Token, tokenize};
use codespan_reporting::diagnostic::Label;
use std::collections::HashSet;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

#[derive(Default)]
pub struct Context<'a> {
    template_start: HashSet<usize>,
    template_end: HashSet<usize>,
    marker: std::marker::PhantomData<&'a ()>,
}

include!(concat!(env!("OUT_DIR"), "/generated.rs"));

impl Parser<'_> {
    fn is_swizzle_name(&self) -> bool {
        let name = self.cst.source[self.span()].as_bytes();
        matches!(
            name,
            [b'r' | b'g' | b'b' | b'a']
                | [b'r' | b'g' | b'b' | b'a', b'r' | b'g' | b'b' | b'a']
                | [
                    b'r' | b'g' | b'b' | b'a',
                    b'r' | b'g' | b'b' | b'a',
                    b'r' | b'g' | b'b' | b'a'
                ]
                | [
                    b'r' | b'g' | b'b' | b'a',
                    b'r' | b'g' | b'b' | b'a',
                    b'r' | b'g' | b'b' | b'a',
                    b'r' | b'g' | b'b' | b'a'
                ]
                | [b'x' | b'y' | b'z' | b'w']
                | [b'x' | b'y' | b'z' | b'w', b'x' | b'y' | b'z' | b'w']
                | [
                    b'x' | b'y' | b'z' | b'w',
                    b'x' | b'y' | b'z' | b'w',
                    b'x' | b'y' | b'z' | b'w'
                ]
                | [
                    b'x' | b'y' | b'z' | b'w',
                    b'x' | b'y' | b'z' | b'w',
                    b'x' | b'y' | b'z' | b'w',
                    b'x' | b'y' | b'z' | b'w'
                ]
        )
    }
    fn find_template_list(&mut self) {
        if self.context.template_start.contains(&self.pos) {
            return;
        }
        let mut nesting_depth = 0usize;
        let mut pending = vec![];
        let mut last_lt_offset = 0;
        for (offset, tok) in self.tokens[self.pos..].iter().enumerate() {
            match tok {
                Token::LPar | Token::LBrak => nesting_depth += 1,
                Token::RPar | Token::RBrak => {
                    while let Some((_, pending_nesting_depth)) = pending.last().copied() {
                        if pending_nesting_depth < nesting_depth {
                            break;
                        } else {
                            pending.pop();
                        }
                    }
                    nesting_depth = nesting_depth.saturating_sub(1);
                }
                Token::Semi | Token::LBrace | Token::Colon => break,
                Token::And2 | Token::Pipe2 => {
                    while let Some((_, pending_nesting_depth)) = pending.last().copied() {
                        if pending_nesting_depth < nesting_depth {
                            break;
                        } else {
                            pending.pop();
                        }
                    }
                }
                Token::Lt | Token::Eq if offset == last_lt_offset + 1 => {
                    pending.pop();
                }
                Token::Lt => {
                    last_lt_offset = offset;
                    pending.push((self.pos + offset, nesting_depth));
                }
                Token::Gt => {
                    if let Some((pending_pos, pending_nesting_depth)) = pending.last().copied() {
                        if pending_nesting_depth == nesting_depth {
                            pending.pop();
                            self.context.template_start.insert(pending_pos);
                            self.context.template_end.insert(self.pos + offset);
                        }
                    }
                }
                _ => {}
            }
            if pending.is_empty() {
                break;
            }
        }
    }
    fn is_func_call(&self) -> bool {
        matches!(self.peek(1), Token::LPar | Token::Lt) && self.peek(2) != Token::Lt
    }
}

impl<'a> ParserCallbacks<'a> for Parser<'a> {
    type Diagnostic = Diagnostic;
    type Context = Context<'a>;

    fn create_tokens(
        _context: &mut Self::Context,
        source: &str,
        diags: &mut Vec<Diagnostic>,
    ) -> (Vec<Token>, Vec<Span>) {
        tokenize(source, diags)
    }
    fn create_diagnostic(&self, span: Span, message: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(message)
            .with_label(Label::primary((), span))
    }
    fn predicate_global_directive_1(&self) -> bool {
        self.peek(1) != Token::Semi
    }
    fn predicate_parameters_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_struct_body_1(&self) -> bool {
        self.peek(1) != Token::RBrace
    }
    fn predicate_attribute_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn action_template_list_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        self.find_template_list();
    }
    fn action_expr_template_list_1(&mut self, _diags: &mut Vec<Diagnostic>) {
        if self.current == Token::Lt {
            self.find_template_list();
        }
    }
    fn predicate_expr_template_list_1(&self) -> bool {
        self.context.template_start.contains(&self.pos)
    }
    fn predicate_template_args_1(&self) -> bool {
        self.peek(1) != Token::Gt
    }
    fn predicate_expression_1(&self) -> bool {
        self.is_swizzle_name()
    }
    fn predicate_expression_2(&self) -> bool {
        if self.current == Token::Gt && self.context.template_end.contains(&self.pos) {
            return false;
        }
        self.tokens[self.pos + 1] == self.current
    }
    fn predicate_expression_3(&self) -> bool {
        self.current != Token::Gt || !self.context.template_end.contains(&self.pos)
    }
    fn predicate_expression_4(&self) -> bool {
        self.tokens[self.pos + 1] != Token::Eq
    }
    fn predicate_argument_expression_list_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_argument_expression_list_expr_1(&self) -> bool {
        self.peek(1) != Token::RPar
    }
    fn predicate_statement_1(&self) -> bool {
        self.peek(1) == Token::If
    }
    fn predicate_statement_2(&self) -> bool {
        self.is_func_call()
    }
    fn predicate_continuing_statement_1(&self) -> bool {
        self.peek(1) != Token::If
    }
    fn predicate_for_init_1(&self) -> bool {
        self.is_func_call()
    }
    fn predicate_for_update_1(&self) -> bool {
        self.is_func_call()
    }
    fn predicate_case_selectors_1(&self) -> bool {
        !matches!(self.peek(1), Token::At | Token::Colon | Token::LBrace)
    }
    fn predicate_compound_assignment_operator_1(&self) -> bool {
        self.tokens[self.pos + 1] == self.current && self.tokens[self.pos + 2] == Token::Eq
    }
    fn predicate_lhs_expression_1(&self) -> bool {
        self.is_swizzle_name()
    }

    fn action_let_decl_1(&mut self, diags: &mut Vec<Self::Diagnostic>) {
        if self.active_error() && self.current == Token::Semi {
            diags
                .last_mut()
                .unwrap()
                .notes
                .push("note: let declaration requires initializer".to_string());
        }
    }
    fn action_const_decl_1(&mut self, diags: &mut Vec<Self::Diagnostic>) {
        if self.active_error() && self.current == Token::Semi {
            diags
                .last_mut()
                .unwrap()
                .notes
                .push("note: const declaration requires initializer".to_string());
        }
    }

    fn create_node_global_let_decl(
        &mut self,
        node_ref: NodeRef,
        diags: &mut Vec<Self::Diagnostic>,
    ) {
        diags.push(
            Diagnostic::error()
                .with_message("global let declarations are not allowed")
                .with_label(Label::primary((), self.cst.span(node_ref))),
        );
    }
}
