fn main() {
    if lelwel::generate(true, false, true, false, "src/json.llw", "src").is_err() {
        std::process::exit(1)
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/json.llw");
}
