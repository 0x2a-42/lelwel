#![no_main]
use lelwel::frontend::parser::{tokenize, Parser, Token};
use lelwel::frontend::sema::SemanticPass;
use logos::Logos;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut diags = vec![];
        let (tokens, ranges) = tokenize(Token::lexer(source), &mut diags);
        let cst = Parser::parse(source, tokens, ranges, &mut diags);
        let _ = SemanticPass::run(&cst, &mut diags);
    }
});
