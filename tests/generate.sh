#!/bin/bash

output=tests/frontend.rs
export NO_COLOR=1

pushd .. > /dev/null

cat << EOF > $output
// This file was generated by the generate.sh script.
// DO NOT EDIT THIS FILE MANUALLY!

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::NoColor;
use codespan_reporting::term::{self, DisplayStyle};
use lelwel::frontend::parser::{tokenize, Parser, Token};
use lelwel::frontend::sema::SemanticPass;
use logos::Logos;
use std::io::BufWriter;

fn gen_diags(input: &str) -> String {
    let source = std::fs::read_to_string(input).unwrap();
    let mut diags = vec![];
    let (tokens, ranges) = tokenize(Token::lexer(&source), &mut diags);
    let cst = Parser::parse(&source, tokens, ranges, &mut diags);
    let _ = SemanticPass::run(&cst, &mut diags);

    let mut writer = NoColor::new(BufWriter::new(Vec::new()));
    let config = codespan_reporting::term::Config {
        display_style: DisplayStyle::Short,
        ..Default::default()
    };
    let file = SimpleFile::new(input, &source);
    for diag in diags {
        term::emit(&mut writer, &config, &file, &diag).unwrap();
    }
    std::str::from_utf8(writer.get_ref().buffer())
        .unwrap()
        .to_string()
}
EOF

for path in tests/frontend/*.llw; do
  path="$(realpath --relative-to=. "$path")"
  file=${path##*/}
  cat << EOF >> $output

#[test]
#[rustfmt::skip]
fn ${file%.llw}() {
    let diags = gen_diags("$path");
    let mut lines = diags.lines();
EOF
  diag=$(llw -cs "$path" 2>&1 > /dev/null)
  echo >> $output
  echo "$diag" | while read -r line ; do
    if [ ! -z "$line" ]; then
      echo "    assert_eq!(lines.next().unwrap(), \"$line\");" >> $output
    fi
  done
  echo '    assert_eq!(lines.next(), None);' >> $output
  echo '}' >> $output
done

popd > /dev/null
