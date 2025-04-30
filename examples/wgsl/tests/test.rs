use lelwel_wgsl::generate_syntax_tree;

macro_rules! check {
    ($file:literal) => {
        let res =
            generate_syntax_tree(&include_str!(concat!("data/", $file, ".wgsl")).replace('\r', ""));
        assert_eq!(
            format!("{}", res[0]),
            include_str!(concat!("data/", $file, ".tree")).replace('\r', "")
        );
        assert_eq!(
            format!("{}", res[1]),
            include_str!(concat!("data/", $file, ".diag")).replace('\r', "")
        );
    };
}

#[test]
fn incomplete() {
    check!("incomplete");
    check!("template");
}
