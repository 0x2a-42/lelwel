use self::parser::{Parser, Token, TokenStream};
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::{
    self,
    termcolor::{ColorChoice, StandardStream},
    Config,
};
use logos::Logos;

mod parser;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }
    let mut tokens = TokenStream::new(Token::lexer(&args[1]));
    let file = SimpleFile::new("stdin", &args[1]);

    let mut diags = vec![];
    if let Some(result) = Parser::parse(&mut tokens, &mut diags) {
        println!("{result}");
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    for diag in diags {
        term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
    }
}
