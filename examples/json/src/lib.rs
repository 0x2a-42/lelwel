mod parser;

use std::io::BufWriter;

use wasm_bindgen::prelude::*;

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::NoColor;
use codespan_reporting::term::{self, Config};
use logos::Logos;
use parser::*;

#[wasm_bindgen]
pub fn generate_syntax_tree(source: &str) -> Vec<String> {
    let mut diags = vec![];
    let (tokens, ranges) = tokenize(Token::lexer(source), &mut diags);
    let cst = Parser::parse(source, tokens, ranges, &mut diags);
    let mut writer = NoColor::new(BufWriter::new(Vec::new()));
    let config = Config::default();
    let file = SimpleFile::new("<input>", source);
    for diag in diags.iter() {
        term::emit(&mut writer, &config, &file, diag).unwrap();
    }
    vec![
        format!("{cst}"),
        String::from_utf8(writer.into_inner().into_inner().unwrap()).unwrap(),
    ]
}
