#![no_main]
use libfuzzer_sys::fuzz_target;

use lelwel::frontend::ast::*;

fuzz_target!(|data: &[u8]| {
    if let Ok(contents) = std::str::from_utf8(data) {
        let ast = Ast::new();
        let _ = lelwel::run_frontend("fuzz.llw", contents.to_string(), &ast);
    }
});
