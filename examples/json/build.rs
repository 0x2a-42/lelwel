fn main() {
    let config = lelwel::Config::new().use_lexer().use_diag();
    if lelwel::generate(config, "src/json.llw", "src").is_err() {
        std::process::exit(1)
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/json.llw");
}
