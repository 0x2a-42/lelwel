use std::process::Command;

fn main() {
    if let Ok(cmd) = Command::new("llw")
        .args(["-o", "src/frontend", "src/frontend/lelwel.llw"])
        .status()
    {
        if !cmd.success() {
            std::process::exit(1)
        }
    }
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=src/frontend/lelwel.llw");
    println!("cargo:rerun-if-changed=src/frontend/parser.rs");
}
