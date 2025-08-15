mod lexer;
mod parser;

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, Config};
use parser::*;
use std::io::{Read, Write};

fn print_diags(path: &str, source: &str, diags: Vec<Diagnostic>) {
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = Config::default();
    let file = SimpleFile::new(path, &source);
    for diag in diags {
        term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
    }
}

fn parse_file(path: &str) -> std::io::Result<()> {
    let mut buf = Vec::new();
    std::fs::File::open(path)?.read_to_end(&mut buf)?;
    let source = if std::str::from_utf8(&buf).is_ok() {
        unsafe { String::from_utf8_unchecked(buf) }
    } else {
        // ISO 8859-1
        buf.iter().map(|&c| c as char).collect()
    };
    let mut diags = vec![];
    let cst = Parser::new(&source, &mut diags).parse(&mut diags);
    println!("{cst}");
    print_diags(path, &source, diags);
    Ok(())
}

fn parse_repl() {
    loop {
        print!(">>> ");
        let _ = std::io::stdout().flush();
        let mut line = String::new();
        match std::io::stdin().read_line(&mut line) {
            Ok(_) => {
                let mut diags = vec![];
                let cst = Parser::new(&line, &mut diags).parse_single_input(&mut diags);
                println!("{cst}");
                print_diags("<input>", &line, diags);
            }
            Err(_) => break,
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    match args.len() {
        1 => {
            parse_repl();
            Ok(())
        }
        2 => parse_file(&args[1]),
        _ => {
            std::process::exit(1);
        }
    }
}
