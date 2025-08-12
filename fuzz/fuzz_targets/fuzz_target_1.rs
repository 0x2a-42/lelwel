#![no_main]
use lelwel::frontend::parser::Parser;
use lelwel::frontend::sema::SemanticPass;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let mut diags = vec![];
        let cst = Parser::new(source, &mut diags).parse(&mut diags);
        let _ = SemanticPass::run(&cst, &mut diags);
    }
});
