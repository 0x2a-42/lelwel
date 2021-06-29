use std::process::Command;

fn main() {
    if !Command::new("llw")
        .args(&["-l", "-o", "src", "src/calc.llw"])
        .status()
        .unwrap()
        .success()
    {
        std::process::exit(1)
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/calc.llw");
}
