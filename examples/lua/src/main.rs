mod ast;
mod parser;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};
use logos::Logos;
use parser::*;
use std::io::Read;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }

    let mut buf = Vec::new();
    std::fs::File::open(&args[1])?.read_to_end(&mut buf)?;
    let source = if std::str::from_utf8(&buf).is_ok() {
        unsafe { String::from_utf8_unchecked(buf) }
    } else {
        // ISO 8859-1
        buf.iter().map(|&c| c as char).collect()
    };

    let mut diags = vec![];
    let (tokens, ranges) = tokenize(Token::lexer(&source), &mut diags);
    let cst = Parser::parse(&source, tokens, ranges, &mut diags);
    println!("{cst}");
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    let file = SimpleFile::new(&args[1], &source);
    for diag in diags.iter() {
        term::emit(&mut writer.lock(), &config, &file, diag).unwrap();
    }
    if diags.iter().any(|d| d.severity == Severity::Error) {
        std::process::exit(1);
    }
    Ok(())
}
