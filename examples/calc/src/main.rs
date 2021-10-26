mod lexer;
mod parser;
mod token;

use std::env;

use lexer::*;
use parser::*;
use token::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return;
    }

    let mut lexer = Lexer::new(args[1].clone(), false);
    match Parser::parse(&mut lexer) {
        Ok(result) => {
            for (range, msg) in lexer.error_iter() {
                eprintln!("{} at {}", msg, range);
            }
            println!("{}", result)
        }
        Err(expect) => eprintln!(
            "syntax error at {}, expected one of {:?}",
            lexer.current().range,
            expect
                .iter()
                .map(TokenKind::to_string)
                .collect::<Vec<String>>()
        ),
    }
}
