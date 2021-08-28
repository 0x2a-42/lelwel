# lelwel
[![Crates.io](https://img.shields.io/crates/v/lelwel)](https://crates.io/crates/lelwel)
[![MIT/Apache 2.0](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](./LICENSE-MIT)
[![Crates.io](https://img.shields.io/crates/d/lelwel)](https://crates.io/crates/lelwel)

[Lelwel](https://en.wikipedia.org/wiki/Lelwel_hartebeest) generates recursive descent parsers for Rust using LL(1) grammars.
Conflicts are resolved with semantic predicates.
Semantic actions are used for ad hoc syntax-directed translation.
A special `error` rule can be used to recover from syntax errors.

Lelwel is written as a library, which is used by the CLI tool `llw` and the language server `lelwel-ls`.
The [parser for lelwel grammar files](src/frontend/lelwel.llw) (\*.llw) is itself generated by lelwel.

There are plugins for Neovim and Visual Studio Code that use the language server.

## Grammar Examples
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
expr#2 { res = res + term; }
expr#3 { res = res - term; }
expr#4 { Ok(res) }

term#1 { let mut res = atomic; }
term#2 { res = res * atomic; }
term#3 { res = res / atomic; }
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
  '{' (member #1 (',' member #2)* | error #3)? '}' #4
;
member{(String, Value)}:
  String ':' value #1
;
array{Value}:
  '[' (value #1 (',' value #2)* | error #3)? ']' #4
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
object#3 { diag.error(e, error_range); }
object#4 { Ok(Value::Object(members)) }

member#1 { Ok((String.0, value)) }

array#0 { let mut values = vec![]; }
array#1 { values.push(value); }
array#2 { values.push(value); }
array#3 { diag.error(e, error_range); }
array#4 { Ok(Value::Array(values)) }
```

## Installation
Run `cargo install --features="cli","lsp" lelwel`.

## License

Lelwel and its generated code is licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
