use lelwel_json::generate_syntax_tree;

macro_rules! check {
    ($file:literal) => {
        let res =
            generate_syntax_tree(&include_str!(concat!("data/", $file, ".json")).replace('\r', ""));
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
fn wrong_key() {
    check!("wrong_key");
}
