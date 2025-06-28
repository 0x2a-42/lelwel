use self::parser::Parser;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{
    self, Config,
    termcolor::{ColorChoice, StandardStream},
};

mod lexer;
mod parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }

    let source = &args[1];
    let mut diags = vec![];
    let cst = Parser::parse(source, &mut diags);
    println!("{cst}");

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    let file = SimpleFile::new(&args[1], source);
    for diag in diags.iter() {
        term::emit(&mut writer.lock(), &config, &file, diag).unwrap();
    }
}
