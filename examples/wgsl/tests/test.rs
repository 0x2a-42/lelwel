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
}
#[test]
fn template() {
    check!("template");
}
#[test]
fn incomplete_if() {
    check!("incomplete_if");
}
#[test]
fn missing_comma() {
    check!("missing_comma");
}
#[test]
fn missing_arrow() {
    check!("missing_arrow");
}
#[test]
fn invalid_for() {
    check!("invalid_for");
}
#[test]
fn incomplete_let() {
    check!("incomplete_let");
}
