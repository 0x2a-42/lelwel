#![forbid(unsafe_code)]

use std::path::Path;

use codespan_reporting::diagnostic::Severity;
use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use codespan_reporting::term::{self, DisplayStyle};

use backend::rust::RustOutput;
use frontend::parser::*;
use frontend::printer::DebugPrinter;
use frontend::sema::*;

use self::backend::graphviz::GraphvizOutput;

pub mod backend;
pub mod frontend;
pub mod ide;

const VERSION: &str = "0.9.1";

pub fn build(path: &str) {
    let res = compile(
        path,
        &std::env::var("OUT_DIR").unwrap(),
        false,
        0,
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

pub fn compile(
    input: &str,
    output: &str,
    check: bool,
    verbose: u8,
    graph: bool,
    short: bool,
) -> std::io::Result<bool> {
    let input_path = Path::new(input);
    input_path.try_exists()?;

    let source = std::fs::read_to_string(input)?;
    let mut diags = vec![];
    let cst = Parser::parse(&source, &mut diags);
    let sema = SemanticPass::run(&cst, &mut diags);

    if verbose > 1 {
        println!("{cst}");
    }
    if verbose > 0 {
        DebugPrinter::new().run(&cst, &sema);
    }
    if !diags.iter().any(|d| d.severity == Severity::Error) {
        if graph {
            GraphvizOutput::run(&cst, &sema)?;
        }
        if !check {
            RustOutput::run(&cst, &sema, input_path, Path::new(output))?;
        }
    }

    let mut success = true;
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let mut config = codespan_reporting::term::Config::default();
    let file = SimpleFile::new(input, &source);
    if short {
        config.display_style = DisplayStyle::Short;
    }
    for diag in diags {
        term::emit(&mut writer.lock(), &config, &file, &diag).unwrap();
        success &= diag.severity != Severity::Error;
    }
    Ok(success)
}

#[cfg(feature = "wasm")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn generate_syntax_tree(source: &str) -> Vec<String> {
    use std::io::BufWriter;

    use codespan_reporting::term::Config;
    use codespan_reporting::term::termcolor::NoColor;

    let mut diags = vec![];
    let cst = Parser::parse(source, &mut diags);
    let _sema = SemanticPass::run(&cst, &mut diags);
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
#[cfg(feature = "wasm")]
#[wasm_bindgen::prelude::wasm_bindgen]
pub fn get_location(source: &str, start: usize, end: usize) -> Vec<usize> {
    use codespan_reporting::files::Files;

    let file = SimpleFile::new("<input>", source);
    let start_loc = file.location((), start).unwrap();
    let end_loc = file.location((), end).unwrap();
    vec![
        start_loc.line_number,
        start_loc.column_number,
        end_loc.line_number,
        end_loc.column_number,
    ]
}
