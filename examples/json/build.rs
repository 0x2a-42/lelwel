use std::process::Command;

fn main() {
    if !Command::new("llw")
        .args(&["-l", "-d", "-o", "src", "src/json.llw"])
        .status()
        .unwrap()
        .success()
    {
        std::process::exit(1)
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/json.llw");
}
