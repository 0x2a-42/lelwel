mod parser;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};
use logos::Logos;
use parser::*;
use std::collections::BTreeMap;

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(String),
    String(String),
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>),
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }

    let mut diags = vec![];
    let contents = std::fs::read_to_string(&args[1])?;

    let mut tokens = TokenStream::new(Token::lexer(&contents));
    let file = SimpleFile::new(&args[1], &contents);

    if let Some(result) = Parser::parse(&mut tokens, &mut diags) {
        println!("{result:?}");
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    for diag in diags.iter() {
        term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
    }
    if diags.iter().any(|d| d.severity == Severity::Error) {
        std::process::exit(1);
    }
    Ok(())
}
