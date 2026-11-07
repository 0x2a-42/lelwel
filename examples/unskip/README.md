# lelwel-unskip

A small parser exercising the **unskip** feature with actual input. Both
`Newline` and `Whitespace` are declared as skip tokens in
[the grammar](src/unskip.llw), but selected rules explicitly consume them.

For example, `single a` must end with either a newline or a comma. Newlines
between `a`, `b`, and `c` in `sequence` are still skipped; the newline after
`c` is consumed as a terminator. The `multiple` rule explicitly accepts
either a newline or horizontal whitespace after `a`.

[The functional tests](tests/unskip.rs) cover required and optional tokens,
zero-or-more and one-or-more repetitions, sequences, nullable rule calls,
multiple unskipped token kinds, ordinary skipping, consecutive statements,
missing required tokens, and skipped terminators inside a repeated rule
(`lines_block`). In `lines_block`, every newline after `lines`' items is a
terminator, so a blank line yields an extra terminator.
Successful parses must have no diagnostics, the expected terminator nodes, and
a lossless CST.

Known limitation: unskip lists are computed from rule-wide follow sets, not per
call site. A newline that `terminator` unskips for `lines_block` also becomes
visible after `single a`, so `single a\n\n` fails. The test
`unskip_does_not_leak_between_statements` is marked `#[ignore]` to document
this; run it with `cargo test -p lelwel-unskip -- --ignored`.

In `single: 'single' A terminator`, the newline in `single a\n` is the terminator. 
A second newline in `single a\n\n` should ideally be skipped.
In `lines_block`, a newline can be followed by another terminator in the repetition. 
There, the parser needs to keep newlines visible so it can recognize blank lines.
The parser doesn’t create one version of terminator for `single` and another for 
`lines_block`. It combines what those uses need into one shared unskip decision. 
Because `lines_block` needs newlines visible, they can remain visible after 
`single` too — where that extra newline causes the parse to fail.

Run from the repository root:

```sh
cargo test -p lelwel-unskip
```
