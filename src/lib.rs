pub mod backend;
pub mod frontend;
pub mod ide;

use crate::{
    backend::rust::*,
    frontend::{ast::*, diag::*, lexer::*, parser::*, sema::*, token::*},
};

#[derive(Clone, Copy)]
pub struct Config(usize);

impl Config {
    pub fn new() -> Self {
        Self(0)
    }
    pub fn use_lexer(&mut self) -> Self {
        self.0 |= 1;
        *self
    }
    pub fn use_symbol(&mut self) -> Self {
        self.0 |= 2;
        *self
    }
    pub fn use_diag(&mut self) -> Self {
        self.0 |= 4;
        *self
    }
    pub fn use_ast(&mut self) -> Self {
        self.0 |= 8;
        *self
    }
    fn has_lexer(&self) -> bool {
        self.0 & 1 != 0
    }
    fn has_symbol(&self) -> bool {
        self.0 & 2 != 0
    }
    fn has_diag(&self) -> bool {
        self.0 & 4 != 0
    }
    fn has_ast(&self) -> bool {
        self.0 & 8 != 0
    }
}

impl Default for Config {
    fn default() -> Self {
        Self::new()
    }
}

pub fn run_frontend<'a>(path: &str, contents: String, ast: &'a Ast<Module<'a>>) -> Diag {
    let mut diag = Diag::new(path, 100);
    let mut lexer = Lexer::new(contents, false);
    if let Err(code) = Parser::parse(&mut lexer, ast, &mut diag) {
        diag.error(code, lexer.current().range);
    }
    for (range, msg) in lexer.error_iter() {
        diag.error(Code::ParserError(msg), *range);
    }
    if let Some(root) = ast.root() {
        SemanticPass::run(root, &mut diag);
    }
    diag
}

pub fn run_backend(
    config: Config,
    ast: &Ast<Module>,
    diag: &Diag,
    output: &str,
    version: &str,
) -> std::io::Result<()> {
    if !diag.has_errors() {
        let path = std::path::Path::new(output);
        let root = ast.root().unwrap();
        match root.language.get() {
            None => {
                output_rust(config, root, path, version)?;
            }
            Some(Element {
                kind: ElementKind::Language { name },
                ..
            }) if name.as_str() == "rust" => {
                output_rust(config, root, path, version)?;
            }
            _ => {}
        }
    }
    Ok(())
}

pub fn output_llw_skel(input: &str) -> std::io::Result<()> {
    let path = std::path::Path::new(input);
    if !path.exists() {
        RustOutput::create_llw_skel(path)?;
    }
    Ok(())
}

pub fn output_rust(
    config: Config,
    root: &Module,
    path: &std::path::Path,
    version: &str,
) -> std::io::Result<()> {
    RustOutput::create_parser(root, path, version)?;
    RustOutput::create_token(path)?;
    if config.has_lexer() {
        RustOutput::create_lexer(path)?;
    }
    if config.has_symbol() {
        RustOutput::create_symbol(path)?;
    }
    if config.has_diag() {
        RustOutput::create_diag(path)?;
    }
    if config.has_ast() {
        RustOutput::create_ast(path)?;
    }
    Ok(())
}

pub fn generate(config: Config, input: &str, output: &str) -> std::io::Result<()> {
    output_llw_skel(input)?;
    let ast = Ast::new();
    let contents = std::fs::read_to_string(input)?;
    let diag = run_frontend(input, contents, &ast);
    run_backend(config, &ast, &diag, output, "")?;

    if !diag.has_errors() {
        Ok(())
    } else {
        diag.print(false);
        Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "invalid lelwel file",
        ))
    }
}
