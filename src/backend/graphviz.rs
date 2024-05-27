use crate::frontend::ast::{AstNode, File, Named, Regex, RuleDecl, TokenDecl};
use crate::{Cst, NodeRef, SemanticData};
use std::io::Write;

pub struct GraphvizOutput;

impl GraphvizOutput {
    pub fn run(cst: &Cst, sema: &SemanticData) -> std::io::Result<()> {
        let mut graph_file = std::fs::File::create("parser.gv")?;
        graph_file.write_all(b"digraph {\n")?;
        if let Some(file) = File::cast(cst, NodeRef::ROOT) {
            for rule in file.rule_decls(cst) {
                Self::visit_rule(cst, sema, rule, &mut graph_file)?;
            }
        }
        graph_file.write_all(b"}\n")
    }

    fn visit_rule(
        cst: &Cst,
        sema: &SemanticData,
        rule: RuleDecl,
        output: &mut std::fs::File,
    ) -> std::io::Result<()> {
        let name = rule.name(cst).unwrap().0;
        output
            .write_all(format!("  \"{}\" [label=\"{}\"];\n", rule.syntax().0, name).as_bytes())?;
        let regex = Self::skip_paren(cst, rule.regex(cst).unwrap());
        Self::visit_regex(cst, sema, regex, output)?;
        output.write_all(
            format!(
                "  \"{}\" -> \"{}\";\n",
                rule.syntax().0,
                Self::skip_name(cst, sema, regex)
            )
            .as_bytes(),
        )
    }

    fn skip_paren(cst: &Cst, regex: Regex) -> Regex {
        match regex {
            Regex::Paren(paren) => Self::skip_paren(cst, paren.inner(cst).unwrap()),
            _ => regex,
        }
    }

    fn escape(input: &str) -> String {
        let mut output = String::new();
        for c in input.chars() {
            match c {
                '|' | '{' | '}' | '<' | '>' => output.push('\\'),
                _ => {}
            }
            output.push(c);
        }
        output
    }

    fn skip_name(cst: &Cst, sema: &SemanticData, regex: Regex) -> String {
        match regex {
            Regex::Name(name) => {
                if let Some(rule) = sema
                    .decl_bindings
                    .get(&name.syntax())
                    .and_then(|decl| RuleDecl::cast(cst, *decl))
                {
                    format!("{}", rule.syntax().0)
                } else {
                    format!("{}", regex.syntax().0)
                }
            }
            _ => format!("{}", regex.syntax().0),
        }
    }

    fn regex_to_string(cst: &Cst, sema: &SemanticData, regex: Regex) -> String {
        match regex {
            Regex::Name(name) => {
                if let Some(value) = sema
                    .decl_bindings
                    .get(&name.syntax())
                    .and_then(|decl| TokenDecl::cast(cst, *decl))
                    .and_then(|token| token.name(cst))
                {
                    value.0.to_string()
                } else {
                    "".to_string()
                }
            }
            Regex::Symbol(symbol) => Self::escape(symbol.value(cst).unwrap().0),
            Regex::Predicate(pred) => pred.value(cst).unwrap().0.to_string(),
            Regex::Action(action) => action.value(cst).unwrap().0.to_string(),
            _ => "".to_string(),
        }
    }

    fn visit_regex(
        cst: &Cst,
        sema: &SemanticData,
        regex: Regex,
        output: &mut std::fs::File,
    ) -> std::io::Result<()> {
        match regex {
            Regex::Name(name) => {
                if let Some(value) = sema
                    .decl_bindings
                    .get(&name.syntax())
                    .and_then(|decl| TokenDecl::cast(cst, *decl))
                    .and_then(|token| token.name(cst))
                {
                    output.write_all(
                        format!(
                            "  \"{}\" [shape=box, label=\"{}\"];\n",
                            name.syntax().0,
                            value.0
                        )
                        .as_bytes(),
                    )
                } else {
                    Ok(())
                }
            }
            Regex::Symbol(symbol) => output.write_all(
                format!(
                    "  \"{}\" [shape=box, label=\"'{}'\"];\n",
                    symbol.syntax().0,
                    Self::escape(symbol.value(cst).unwrap().0)
                )
                .as_bytes(),
            ),
            Regex::Concat(concat) => {
                let mut s = String::new();
                for (i, op) in concat.operands(cst).enumerate() {
                    let op = Self::skip_paren(cst, op);
                    if i > 0 {
                        s += "|"
                    }
                    s += &format!("<f{}>{}", i, Self::regex_to_string(cst, sema, op));
                }
                output.write_all(
                    format!(
                        "  \"{}\" [shape=record, label=\"{}\"];\n",
                        concat.syntax().0,
                        s
                    )
                    .as_bytes(),
                )?;
                for (i, op) in concat.operands(cst).enumerate() {
                    let op = Self::skip_paren(cst, op);
                    if Self::regex_to_string(cst, sema, op).is_empty() {
                        Self::visit_regex(cst, sema, op, output)?;
                        output.write_all(
                            format!(
                                "  \"{}\":f{} -> \"{}\";\n",
                                concat.syntax().0,
                                i,
                                Self::skip_name(cst, sema, op)
                            )
                            .as_bytes(),
                        )?;
                    }
                }
                Ok(())
            }
            Regex::Alternation(alt) => {
                output.write_all(
                    format!(
                        "  \"{}\" [shape=record, label=\"{{\\|}}\"];\n",
                        alt.syntax().0
                    )
                    .as_bytes(),
                )?;
                for op in alt.operands(cst) {
                    let op = Self::skip_paren(cst, op);
                    Self::visit_regex(cst, sema, op, output)?;
                    output.write_all(
                        format!(
                            "  \"{}\" -> \"{}\";\n",
                            alt.syntax().0,
                            Self::skip_name(cst, sema, op)
                        )
                        .as_bytes(),
                    )?;
                }
                Ok(())
            }
            Regex::Star(star) => {
                output.write_all(
                    format!("  \"{}\" [shape=box, label=\"*\"];\n", star.syntax().0).as_bytes(),
                )?;
                let op = Self::skip_paren(cst, star.operand(cst).unwrap());
                Self::visit_regex(cst, sema, op, output)?;
                output.write_all(
                    format!(
                        "  \"{}\" -> \"{}\";\n",
                        star.syntax().0,
                        Self::skip_name(cst, sema, op)
                    )
                    .as_bytes(),
                )
            }
            Regex::Plus(plus) => {
                output.write_all(
                    format!("  \"{}\" [shape=box, label=\"+\"];\n", plus.syntax().0).as_bytes(),
                )?;
                let op = Self::skip_paren(cst, plus.operand(cst).unwrap());
                Self::visit_regex(cst, sema, op, output)?;
                output.write_all(
                    format!(
                        "  \"{}\" -> \"{}\";\n",
                        plus.syntax().0,
                        Self::skip_name(cst, sema, op)
                    )
                    .as_bytes(),
                )
            }
            Regex::Optional(opt) => {
                output.write_all(
                    format!("  \"{}\" [shape=box, label=\"[]\"];\n", opt.syntax().0).as_bytes(),
                )?;
                let op = Self::skip_paren(cst, opt.operand(cst).unwrap());
                Self::visit_regex(cst, sema, op, output)?;
                output.write_all(
                    format!(
                        "  \"{}\" -> \"{}\";\n",
                        opt.syntax().0,
                        Self::skip_name(cst, sema, op)
                    )
                    .as_bytes(),
                )
            }
            Regex::Action(action) => output.write_all(
                format!(
                    "  \"{}\" [shape=box, label=\"#{}\"];\n",
                    action.syntax().0,
                    action.value(cst).unwrap().0
                )
                .as_bytes(),
            ),
            Regex::Predicate(pred) => output.write_all(
                format!(
                    "  \"{}\" [shape=box, label=\"?{}\"];\n",
                    pred.syntax().0,
                    pred.value(cst).unwrap().0
                )
                .as_bytes(),
            ),
            _ => Ok(()),
        }
    }
}
