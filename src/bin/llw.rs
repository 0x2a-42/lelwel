#![cfg(feature = "cli")]

use atty::Stream;
use clap::{crate_name, crate_version, arg, App, ArgMatches};
use lelwel::{
    backend::graphviz::*,
    frontend::{diag::*, printer::*},
};

fn translate(matches: ArgMatches) -> std::io::Result<()> {
    if let Some(input) = matches.value_of("INPUT") {
        let with_lexer = matches.is_present("lexer");
        let with_symbol = matches.is_present("symbol");
        let with_diag = matches.is_present("diag");
        let with_ast = matches.is_present("ast");
        let output = matches.value_of("output").unwrap_or(".");
        lelwel::output_llw_skel(with_diag, with_ast, input)?;
        let (ast, diag) = lelwel::llw_to_ast(input)?;

        if let Some(root) = ast.root() {
            if matches.is_present("verbose") {
                let mut printer = DebugPrinter::new();
                printer.visit(root);
            }
            if !diag.has_errors() {
                if matches.is_present("graph") {
                    GraphvizOutput::visit(root)?;
                }
            }
        }

        if !matches.is_present("check") {
            lelwel::ast_to_code(with_lexer, with_symbol, with_diag, with_ast, &ast, &diag, output, crate_version!())?;
        }

        diag.print(atty::is(Stream::Stderr));
        if diag.has_errors() {
            std::process::exit(1);
        }
    } else {
        std::process::exit(1);
    }
    Ok(())
}

fn main() {
    let matches = App::new(crate_name!())
        .max_term_width(80)
        .version(crate_version!())
        .about("Generates recursive descent parsers for Rust using LL(1) grammars.")
        .arg(arg!(-c --check         "Only check the file for errors"))
        .arg(arg!(-l --lexer         "Generate a lexer skeleton"))
        .arg(arg!(-s --symbol        "Generate a symbol handling skeleton"))
        .arg(arg!(-d --diag          "Generate a diagnostic handling skeleton"))
        .arg(arg!(-a --ast           "Generate an AST skeleton (implies -d)"))
        .arg(arg!(-g --graph         "Output a graphviz file for the grammar"))
        .arg(arg!(<INPUT>            "Sets the input file to use"))
        .arg(arg!(-v --verbose       "Sets the level of verbosity"))
        .arg(arg!(-o --output <FILE> "Sets the output directory").required(false))
        .after_help("Report bugs to <https://github.com/0x2a-42/lelwel>.")
        .get_matches();

    if let Err(e) = translate(matches) {
        Diag::fatal_error(&format!("{}", e), atty::is(Stream::Stderr));
        std::process::exit(1);
    }
}
