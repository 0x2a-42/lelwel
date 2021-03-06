# lelwel
[![Crates.io](https://img.shields.io/crates/v/lelwel)](https://crates.io/crates/lelwel)
[![MIT/Apache 2.0](https://img.shields.io/crates/l/lelwel)](./LICENSE-MIT)
[![Crates.io](https://img.shields.io/crates/d/lelwel)](https://crates.io/crates/lelwel)
[![Rust](https://img.shields.io/github/workflow/status/0x2a-42/lelwel/Tests)](https://github.com/0x2a-42/lelwel/actions)

[Lelwel](https://en.wikipedia.org/wiki/Lelwel_hartebeest) generates recursive descent parsers for Rust using [LL(1) grammars](https://en.wikipedia.org/wiki/LL_grammar).
Conflicts are resolved with semantic predicates.
Semantic actions are used for ad hoc syntax-directed translation.
Unlike in other parser generators (e.g. Bison, JavaCC, or Coco/R), actions and predicates are not defined inline, which makes it easier to read the grammar.

Lelwel is written as a library, which is used by the CLI tool `llw` and the language server `lelwel-ls`.
There are plugins for [Neovim](https://github.com/0x2a-42/nvim-lelwel) and Visual Studio Code (not published yet) that use the language server.

## Why Yet Another Parser Generator?
* **Language Server:** Get instant feedback when your grammar contains conflicts or errors.
* **Easy to Read:** Code for semantic actions and predicates does not clutter the grammar.
* **Easy to Debug:** The generated parser is easy to understand and can be debugged with standard tools.


## Installation
Run `cargo install --features="cli","lsp" lelwel`.

## Grammar Examples
The [parser for lelwel grammar files](src/frontend/lelwel.llw) (\*.llw) is itself generated by lelwel.
The following examples show grammars for a [basic calculator](examples/calc) and [JSON](examples/json).

### Calculator
```antlr
// token definitions

token Num{f64}='number';
token Add='+' Sub='-' Mul='*' Div='/';
token LPar='(' RPar=')';

// grammar rules

start{f64}:
  expr #1
;
expr{f64}:
  term #1 (
    '+' term #2
  | '-' term #3
  )* #4
;
term{f64}:
  atomic #1 (
    '*' atomic #2
  | '/' atomic #3
  )* #4
;
atomic{f64}:
  Num #1
| '(' expr ')' #2
;

// semantic actions

start#1 { Ok(expr) }

expr#1 { let mut res = term; }
expr#2 { res += term; }
expr#3 { res -= term; }
expr#4 { Ok(res) }

term#1 { let mut res = atomic; }
term#2 { res *= atomic; }
term#3 { res /= atomic; }
term#4 { Ok(res) }

atomic#1 { Ok(Num.0) }
atomic#2 { Ok(expr) }
```

### JSON
```antlr
token True='true' False='false' Null='null';
token LBrace='{' RBrace='}' LBrak='[' RBrak=']' Comma=',' Colon=':';
token String{String}='<string>' Number{String}='<number>';

start{Value}:
  value #1
;
value{Value}:
  object #1
| array #2
| String #3
| Number #4
| 'true' #5
| 'false' #6
| 'null' #7
;
object{Value}:
  '{' [member #1 (',' member #2)* | !1] '}' #3
;
member{(String, Value)}:
  String ':' value #1
;
array{Value}:
  '[' [value #1 (',' value #2)* | !1] ']' #3
;

limit 1000;

preamble {
    use std::collections::BTreeMap;
    use super::diag::*;
    use super::Value;
}
parameters { diag: &mut Diag }
error { Code }

start#1 { Ok(value) }

value#1 { Ok(object) }
value#2 { Ok(array) }
value#3 { Ok(Value::String(String.0)) }
value#4 { Ok(Value::Number(Number.0)) }
value#5 { Ok(Value::Bool(true)) }
value#6 { Ok(Value::Bool(false)) }
value#7 { Ok(Value::Null) }

object#0 { let mut members = BTreeMap::new(); }
object#1 { members.insert(member.0, member.1); }
object#2 { members.insert(member.0, member.1); }
object#3 { Ok(Value::Object(members)) }
object!1 { diag.error(error_code, error_range); }

member#1 { Ok((String.0, value)) }

array#0 { let mut values = vec![]; }
array#1 { values.push(value); }
array#2 { values.push(value); }
array#3 { Ok(Value::Array(values)) }
array!1 { diag.error(error_code, error_range); }
```

## License

Lelwel and its generated code is licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
