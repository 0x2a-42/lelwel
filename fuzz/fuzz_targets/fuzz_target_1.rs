#![no_main]
use libfuzzer_sys::fuzz_target;

use lelwel::frontend::ast::*;
use lelwel::frontend::diag::*;
use lelwel::frontend::lexer::*;
use lelwel::frontend::sema::*;

fuzz_target!(|data: &[u8]| {
    if let Ok(contents) = std::str::from_utf8(data) {
        let mut lexer = Lexer::new(contents.to_string(), false);
        let mut diag = Diag::new("fuzz.llw", 100);
        let ast = Ast::new(&mut lexer, &mut diag);
        if let Some(root) = ast.root() {
            SemanticPass::run(root, &mut diag);
        }
    }
});
