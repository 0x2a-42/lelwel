pub mod backend;
pub mod frontend;
pub mod ide;

use crate::{
    backend::rust::*,
    frontend::{ast::*, diag::*, lexer::*, sema::*},
};

pub fn llw_to_ast(
    path: &str,
) -> std::io::Result<(Ast<Lexer>, Diag)> {
    let contents = std::fs::read_to_string(path)?;
    let mut diag = Diag::new(path, 100);
    let mut lexer = Lexer::new(contents, false);
    let ast = Ast::new(&mut lexer, &mut diag);

    for (range, msg) in lexer.error_iter() {
        diag.error(Code::ParserError(msg), *range);
    }
    if let Some(root) = ast.root() {
        SemanticPass::run(root, &mut diag);
    }
    Ok((ast, diag))
}

pub fn ast_to_code(
    with_lexer: bool,
    with_symbol: bool,
    with_diag: bool,
    with_ast: bool,
    ast: &Ast<Lexer>,
    diag: &Diag,
    output: &str,
    version: &str,
) -> std::io::Result<()> {
    if let Some(root) = ast.root() {
        if !diag.has_errors() {
            let path = std::path::Path::new(output);
            match root.language.get() {
                None => {
                    output_rust(with_lexer, with_symbol, with_diag, with_ast, root, path, version)?;
                }
                Some(Element {
                    kind: ElementKind::Language { name },
                    ..
                }) if name.as_str() == "rust" => {
                    output_rust(with_lexer, with_symbol, with_diag, with_ast, root, path, version)?;
                }
                _ => {}
            }
        }
    }
    Ok(())
}

pub fn output_llw_skel(
    with_diag: bool,
    with_ast: bool,
    input: &str,
) -> std::io::Result<()> {
    let path = std::path::Path::new(input);
    if !path.exists() {
        RustOutput::create_llw_skel(
            path,
            with_ast,
            with_diag,
        )?;
    }
    Ok(())
}

pub fn output_rust(
    with_lexer: bool,
    with_symbol: bool,
    with_diag: bool,
    with_ast: bool,
    root: &Module,
    path: &std::path::Path,
    version: &str,
) -> std::io::Result<()> {
    RustOutput::create_parser(root, path, version)?;
    RustOutput::create_token(path)?;
    if with_lexer {
        RustOutput::create_lexer(path)?;
    }
    if with_symbol {
        RustOutput::create_symbol(path)?;
    }
    if with_diag || with_ast {
        RustOutput::create_diag(path)?;
    }
    if with_ast {
        RustOutput::create_ast(path)?;
    }
    Ok(())
}

pub fn generate(
    with_lexer: bool,
    with_symbol: bool,
    with_diag: bool,
    with_ast: bool,
    input: &str,
    output: &str,
) -> std::io::Result<()> {
    output_llw_skel(with_diag, with_ast, input)?;
    let (ast, diag) = llw_to_ast(input)?;
    ast_to_code(with_lexer, with_symbol, with_diag, with_ast, &ast, &diag, output, "")?;

    if !diag.has_errors() {
        Ok(())
    } else {
        diag.print(false);
        Err(std::io::Error::new(std::io::ErrorKind::Other, "invalid lelwel file"))
    }
}
