use lelwel_oberon0::generate_syntax_tree;

macro_rules! check {
    ($file:literal) => {
        let res = generate_syntax_tree(include_str!(concat!("data/", $file, ".mod")));
        assert_eq!(
            format!("{}", res[0]),
            include_str!(concat!("data/", $file, ".tree"))
        );
        assert_eq!(
            format!("{}", res[1]),
            include_str!(concat!("data/", $file, ".diag"))
        );
    };
}

#[test]
fn incomplete() {
    check!("incomplete");
}
