#![cfg(feature = "cli")]

use atty::Stream;
use clap::{crate_name, crate_version, App, ArgMatches};
use lelwel::backend::rust::*;
use lelwel::frontend::ast::*;
use lelwel::frontend::diag::*;
use lelwel::frontend::lexer::*;
use lelwel::frontend::printer::*;
use lelwel::frontend::sema::*;

fn output_rust(matches: &ArgMatches, root: &Module, path: &std::path::Path) -> std::io::Result<()> {
    RustOutput::create_parser(root, path, crate_version!())?;
    RustOutput::create_token(path)?;
    if matches.is_present("lexer") {
        RustOutput::create_lexer(path)?;
    }
    if matches.is_present("symbol") {
        RustOutput::create_symbol(path)?;
    }
    if matches.is_present("diag") || matches.is_present("ast") {
        RustOutput::create_diag(path)?;
    }
    if matches.is_present("ast") {
        RustOutput::create_ast(path)?;
    }
    Ok(())
}

fn translate(matches: ArgMatches) -> std::io::Result<()> {
    if let Some(input) = matches.value_of("INPUT") {
        let path = std::path::Path::new(input);
        if !path.exists() {
            RustOutput::create_parser_skel(
                path,
                matches.is_present("ast"),
                matches.is_present("diag"),
            )?;
        }
        let contents = std::fs::read_to_string(path)?;
        let mut diag = Diag::new(&input, 100);
        let mut lexer = Lexer::new(contents, false);
        let ast = Ast::new(&mut lexer, &mut diag);

        if let Some(root) = ast.root() {
            SemanticPass::run(root, &mut diag);
            match matches.occurrences_of("v") {
                0 => {}
                _ => {
                    let mut printer = DebugPrinter::new();
                    printer.visit(root);
                }
            }
            if !diag.has_errors() && !matches.is_present("check") {
                let output = matches.value_of("output").unwrap_or(".");
                let path = std::path::Path::new(output);

                match root.language.get() {
                    None => {
                        output_rust(&matches, root, &path)?;
                    }
                    Some(Element {
                        kind: ElementKind::Language { name },
                        ..
                    }) if name.as_str() == "rust" => {
                        output_rust(&matches, root, &path)?;
                    }
                    _ => {}
                }
            }
        }

        for tok in lexer.invalid_iter() {
            diag.error(Code::ParserError("invalid token"), tok.range);
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
        .about(
            "Generates recursive descent parsers for Rust using LL(1) grammars. \
             Conflicts are resolved with semantic predicates. \
             Semantic actions are used for ad hoc syntax-directed translation.",
        )
        .arg("-c, --check         'Only check the file for errors'")
        .arg("-l, --lexer         'Generate a lexer skeleton'")
        .arg("-s, --symbol        'Generate a symbol handling skeleton'")
        .arg("-d, --diag          'Generate a diagnostic handling skeleton'")
        .arg("-a, --ast           'Generate an AST skeleton (implies -d)'")
        .arg("<INPUT>             'Sets the input file to use'")
        .arg("-v...               'Sets the level of verbosity'")
        .arg("-o, --output=[FILE] 'Sets the output directory'")
        .after_help("Report bugs to <https://github.com/0x2a-42/lelwel>.")
        .get_matches();

    match translate(matches) {
        Err(e) => {
            Diag::fatal_error(&format!("{}", e), atty::is(Stream::Stderr));
            std::process::exit(1);
        }
        _ => {}
    }
}
