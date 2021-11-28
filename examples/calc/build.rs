fn main() {
    if lelwel::generate(true, false, false, false, "src/calc.llw", "src").is_err() {
        std::process::exit(1)
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/calc.llw");
}
