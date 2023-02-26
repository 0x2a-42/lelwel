use crate::frontend::ast::*;
use std::fs::*;
use std::io::Write;

pub struct GraphvizOutput;

impl GraphvizOutput {
    pub fn visit(module: &Module) -> std::io::Result<()> {
        let mut file = File::create("parser.gv")?;
        file.write_all(b"digraph {\n")?;
        for element in module.elements.iter() {
            Self::visit_element(element, &mut file)?;
        }
        file.write_all(b"}\n")
    }

    fn visit_element(element: &Element, output: &mut File) -> std::io::Result<()> {
        match &element.kind {
            ElementKind::Start { regex, .. } => {
                output.write_all(format!("  \"{:p}\" [label=\"start\"];\n", element).as_bytes())?;
                let regex = Self::skip_paren(regex);
                Self::visit_regex(regex, output)?;
                output.write_all(
                    format!("  \"{:p}\" -> \"{}\";\n", element, Self::skip_id(regex)).as_bytes(),
                )
            }
            ElementKind::Rule { name, regex, .. } => {
                output
                    .write_all(format!("  \"{:p}\" [label=\"{}\"];\n", element, name).as_bytes())?;
                let regex = Self::skip_paren(regex);
                Self::visit_regex(regex, output)?;
                output.write_all(
                    format!("  \"{:p}\" -> \"{}\";\n", element, Self::skip_id(regex)).as_bytes(),
                )
            }
            _ => Ok(()),
        }
    }

    fn skip_paren<'a>(regex: &'a Regex<'a>) -> &'a Regex<'a> {
        match &regex.kind {
            RegexKind::Paren { op } => Self::skip_paren(op),
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

    fn skip_id(regex: &Regex) -> String {
        match &regex.kind {
            RegexKind::Id { elem, .. } => match elem.get().unwrap().kind {
                ElementKind::Rule { .. } => format!("{:p}", elem.get().unwrap()),
                _ => format!("{:p}", regex),
            },
            _ => format!("{:p}", regex),
        }
    }

    fn regex_to_string(regex: &Regex) -> String {
        match &regex.kind {
            RegexKind::Id { elem, .. } => match elem.get().unwrap().kind {
                ElementKind::Token { name, .. } => name.as_string(),
                _ => "".to_string(),
            },
            RegexKind::Str { val, .. } => {
                format!("'{}'", Self::escape(val.as_str()))
            }
            RegexKind::Predicate { val, .. } => {
                format!("?{}", val)
            }
            RegexKind::Action { val, .. } => {
                format!("#{}", val)
            }
            RegexKind::ErrorHandler { val, .. } => {
                format!("!{}", val)
            }
            _ => "".to_string(),
        }
    }

    fn visit_regex(regex: &Regex, output: &mut File) -> std::io::Result<()> {
        match &regex.kind {
            RegexKind::Id { elem, .. } => match elem.get().unwrap().kind {
                ElementKind::Token { name, .. } => output.write_all(
                    format!("  \"{:p}\" [shape=box, label=\"{}\"];\n", regex, name).as_bytes(),
                ),
                _ => Ok(()),
            },
            RegexKind::Str { val, .. } => output.write_all(
                format!(
                    "  \"{:p}\" [shape=box, label=\"'{}'\"];\n",
                    regex,
                    Self::escape(val.as_str())
                )
                .as_bytes(),
            ),
            RegexKind::Concat { ops, .. } => {
                let mut s = String::new();
                for (i, op) in ops.iter().enumerate() {
                    let op = Self::skip_paren(op);
                    if i > 0 {
                        s += "|"
                    }
                    s += &format!("<f{}>{}", i, Self::regex_to_string(op));
                }
                output.write_all(
                    format!("  \"{:p}\" [shape=record, label=\"{}\"];\n", regex, s).as_bytes(),
                )?;
                for (i, op) in ops.iter().enumerate() {
                    let op = Self::skip_paren(op);
                    if Self::regex_to_string(op).is_empty() {
                        Self::visit_regex(op, output)?;
                        output.write_all(
                            format!("  \"{:p}\":f{} -> \"{}\";\n", regex, i, Self::skip_id(op))
                                .as_bytes(),
                        )?;
                    }
                }
                Ok(())
            }
            RegexKind::Or { ops, error } => {
                let s = if let Some(Regex {
                    kind: RegexKind::ErrorHandler { val, .. },
                    ..
                }) = error.get()
                {
                    format!("\\||!{}", val)
                } else {
                    "\\|".to_string()
                };
                output.write_all(
                    format!("  \"{:p}\" [shape=record, label=\"{{{}}}\"];\n", regex, s).as_bytes(),
                )?;
                for op in ops {
                    let op = Self::skip_paren(op);
                    if let RegexKind::ErrorHandler { .. } = op.kind {
                        continue;
                    }
                    Self::visit_regex(op, output)?;
                    output.write_all(
                        format!("  \"{:p}\" -> \"{}\";\n", regex, Self::skip_id(op)).as_bytes(),
                    )?;
                }
                Ok(())
            }
            RegexKind::Star { op } => {
                output.write_all(
                    format!("  \"{:p}\" [shape=box, label=\"*\"];\n", regex).as_bytes(),
                )?;
                let op = Self::skip_paren(op);
                Self::visit_regex(op, output)?;
                output.write_all(
                    format!("  \"{:p}\" -> \"{}\";\n", regex, Self::skip_id(op)).as_bytes(),
                )
            }
            RegexKind::Plus { op } => {
                output.write_all(
                    format!("  \"{:p}\" [shape=box, label=\"+\"];\n", regex).as_bytes(),
                )?;
                let op = Self::skip_paren(op);
                Self::visit_regex(op, output)?;
                output.write_all(
                    format!("  \"{:p}\" -> \"{}\";\n", regex, Self::skip_id(op)).as_bytes(),
                )
            }
            RegexKind::Option { op } => {
                output.write_all(
                    format!("  \"{:p}\" [shape=box, label=\"[]\"];\n", regex).as_bytes(),
                )?;
                let op = Self::skip_paren(op);
                Self::visit_regex(op, output)?;
                output.write_all(
                    format!("  \"{:p}\" -> \"{}\";\n", regex, Self::skip_id(op)).as_bytes(),
                )
            }
            RegexKind::Action { val, .. } => output.write_all(
                format!("  \"{:p}\" [shape=box, label=\"#{}\"];\n", regex, val,).as_bytes(),
            ),
            RegexKind::Predicate { val, .. } => output.write_all(
                format!("  \"{:p}\" [shape=box, label=\"?{}\"];\n", regex, val,).as_bytes(),
            ),
            _ => Ok(()),
        }
    }
}
