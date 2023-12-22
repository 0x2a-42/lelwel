use std::fs::File;
use std::io::Write;
use std::path::Path;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, DisplayStyle};
use logos::Logos;

use backend::rust::RustOutput;
use frontend::parser::*;
use frontend::printer::DebugPrinter;
use frontend::sema::*;
use frontend::symbols::StringInterner;

use self::backend::graphviz::GraphvizOutput;

pub mod backend;
pub mod frontend;
pub mod ide;

const VERSION: &str = "0.5.0";

pub fn build(path: &str) {
    let res = compile(
        path,
        &std::env::var("OUT_DIR").unwrap(),
        false,
        false,
        false,
        false,
    );
    match res {
        Err(err) => {
            eprintln!("{err}");
            std::process::exit(1);
        }
        Ok(false) => std::process::exit(1),
        Ok(true) => {}
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={path}");
}

fn create_new_llw(path: &Path) -> std::io::Result<()> {
    let mut file = File::create(path)?;
    file.write_all(
        b"start:\
        \n;",
    )
}

pub fn compile(
    input: &str,
    output: &str,
    check: bool,
    verbose: bool,
    graph: bool,
    short: bool,
) -> std::io::Result<bool> {
    let input_path = Path::new(input);
    if !check && !input_path.exists() {
        create_new_llw(input_path)?;
    }

    let content = std::fs::read_to_string(input)?;
    let mut tokens = TokenStream::new(Token::lexer(&content), input);

    let file = SimpleFile::new(input, &content);

    let mut diags = vec![];
    let mut interner = StringInterner::new();

    if let Some(mut module) = Parser::parse(&mut tokens, &mut diags, &mut interner) {
        SemanticPass::run(&mut module, &mut diags);
        if verbose {
            DebugPrinter::new().visit(&module);
        }
        if !diags.iter().any(|d| d.severity == Severity::Error) {
            if graph {
                GraphvizOutput::visit(&module)?;
            }
            if !check {
                RustOutput::create(&module, input_path, Path::new(output))?;
            }
        }
    }

    let mut success = true;
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let mut config = codespan_reporting::term::Config::default();
    if short {
        config.display_style = DisplayStyle::Short;
    }
    for diag in diags {
        term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
        success &= diag.severity != Severity::Error;
    }
    Ok(success)
}
