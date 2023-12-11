#![no_main]
use lelwel::frontend::parser::{TokenStream, Token, Parser};
use lelwel::frontend::sema::SemanticPass;
use lelwel::frontend::symbols::StringInterner;
use logos::Logos;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(content) = std::str::from_utf8(data) {
        let mut tokens = TokenStream::new(Token::lexer(content), "fuzz.llw");
        let mut diags = vec![];
        let mut interner = StringInterner::new();
        if let Some(mut module) = Parser::parse(&mut tokens, &mut diags, &mut interner) {
            SemanticPass::run(&mut module, &mut diags);
        }
    }
});
