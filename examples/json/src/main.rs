mod diag;
mod lexer;
#[rustfmt::skip]
mod parser;
mod token;

use diag::*;
use lexer::*;
use parser::*;
use std::collections::BTreeMap;
use std::env;
use token::*;

#[derive(Debug)]
pub enum Value {
    Null,
    Bool(bool),
    Number(String),
    String(String),
    Array(Vec<Value>),
    Object(BTreeMap<String, Value>),
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        std::process::exit(1);
    }

    let mut diag = Diag::new(&args[1], 100);
    if let Ok(contents) = std::fs::read_to_string(&args[1]) {
        let mut lexer = Lexer::new(contents, false);
        let root = Parser::parse(&mut lexer, &mut diag);
        for tok in lexer.invalid_iter() {
            diag.error(Code::ParserError("invalid token"), tok.range);
        }
        match root {
            Ok(root) => println!("{:#?}", root),
            Err(e) => diag.error(e, lexer.current().range)
        }
    } else {
        diag.error(
            Code::ParserError("stream did not contain valid UTF-8"),
            Range::default(),
        )
    }
    diag.print();
    if diag.has_errors() {
        std::process::exit(1);
    }
}
