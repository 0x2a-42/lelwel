#![cfg(feature = "cli")]

use atty::Stream;
use clap::{arg, crate_name, crate_version, ArgMatches, Command};
use lelwel::{
    backend::graphviz::*,
    frontend::{ast::*, diag::*, printer::*},
};

fn translate(matches: ArgMatches) -> std::io::Result<()> {
    if let Some(input) = matches.get_one::<String>("INPUT") {
        let mut config = lelwel::Config::new();
        if matches.get_flag("lexer") {
            config.use_lexer();
        }
        if matches.get_flag("symbol") {
            config.use_symbol();
        }
        if matches.get_flag("diag") {
            config.use_diag();
        }
        if matches.get_flag("ast") {
            config.use_ast();
        }
        let output = matches.get_one::<String>("output").unwrap();
        lelwel::output_llw_skel(input)?;
        let ast = Ast::new();
        let contents = std::fs::read_to_string(input)?;
        let diag = lelwel::run_frontend(input, contents, &ast);

        if let Some(root) = ast.root() {
            if matches.get_flag("verbose") {
                let mut printer = DebugPrinter::new();
                printer.visit(root);
            }
            if !diag.has_errors() && matches.get_flag("graph") {
                GraphvizOutput::visit(root)?;
            }
        }

        if !matches.get_flag("check") {
            lelwel::run_backend(config, &ast, &diag, output, crate_version!())?;
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
    let matches = Command::new(crate_name!())
        .max_term_width(80)
        .version(crate_version!())
        .about("Generates recursive descent parsers for Rust using LL(1) grammars.")
        .arg(arg!(-c --check         "Only check the file for errors"))
        .arg(arg!(-l --lexer         "Generate a lexer skeleton"))
        .arg(arg!(-s --symbol        "Generate a symbol handling skeleton"))
        .arg(arg!(-d --diag          "Generate a diagnostic handling skeleton"))
        .arg(arg!(-a --ast           "Generate an AST skeleton"))
        .arg(arg!(-g --graph         "Output a graphviz file for the grammar"))
        .arg(arg!(<INPUT>            "Sets the input file to use"))
        .arg(arg!(-v --verbose       "Sets the level of verbosity"))
        .arg(arg!(-o --output <FILE> "Sets the output directory").default_value(".").required(false))
        .after_help("Report bugs to <https://github.com/0x2a-42/lelwel>.")
        .get_matches();

    if let Err(e) = translate(matches) {
        Diag::fatal_error(&format!("{}", e), atty::is(Stream::Stderr));
        std::process::exit(1);
    }
}
