mod ast;
mod lexer;
mod parser;

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};
use parser::*;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }

    let source = std::fs::read_to_string(&args[1]).unwrap();
    let mut diags = vec![];
    let cst = Parser::parse(&source, &mut diags);
    println!("{cst}");
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    let file = SimpleFile::new(&args[1], &source);
    for diag in diags.iter() {
        term::emit(&mut writer.lock(), &config, &file, diag).unwrap();
    }
}
