# lelwel
[![Crates.io](https://img.shields.io/crates/v/lelwel)](https://crates.io/crates/lelwel)
[![MIT/Apache 2.0](https://img.shields.io/crates/l/lelwel)](./LICENSE-MIT)
[![Crates.io](https://img.shields.io/crates/d/lelwel)](https://crates.io/crates/lelwel)
[![Rust](https://img.shields.io/github/actions/workflow/status/0x2a-42/lelwel/rust.yml)](https://github.com/0x2a-42/lelwel/actions)
[![Playground](https://img.shields.io/badge/playground-8A2BE2)](https://0x2a-42.github.io/playground.html)

## Table of Contents
* [Introduction](#introduction)
* [Error Resilience](#error-resilience)
* [Grammar Examples](#grammar-examples)
* [Quickstart](#quickstart)
* [Grammar Specification](#grammar-specification)
* [License](#license)

## Introduction

[Lelwel](https://en.wikipedia.org/wiki/Lelwel_hartebeest) (**L**anguage for **E**xtended **L**L(1) parsing **W**ith **E**rror resilience and **L**ossless syntax trees) generates recursive descent parsers for Rust using [LL(1) grammars](https://en.wikipedia.org/wiki/LL_grammar) with extensions for direct left recursion, operator precedence, semantic predicates (which also enable arbitrary lookahead), semantic actions (which allow to deal with semantic context sensitivity, e.g. type / variable name ambiguity in C), and a restricted ordered choice (which allows for backtracking).

The parser creates a homogeneous, lossless, concrete syntax tree (CST) that can be used to construct an abstract syntax tree (AST).
Special node rename, elision, marker, and creation operators allow fine-grained control over how the CST is built for certain parses.

The error recovery and tree construction is inspired by Alex Kladov's (matklad) [Resilient LL Parsing Tutorial](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html).
Lelwel uses a (to my knowledge) novel heuristic to automatically calculate the recovery sets, by using the follow sets of the dominators in the directed graph induced by the grammar.

Lelwel is written as a library.
It is used by the CLI tool `llw`, the language server `lelwel-ls`, and can be included as a build dependency in order to be called from a `build.rs` file.
There is a plugin for [Neovim](https://github.com/0x2a-42/nvim-lelwel) that uses the language server.

By default the generated parser uses [Logos](https://github.com/maciejhirsz/logos) for lexing and [Codespan](https://github.com/brendanzab/codespan) for diagnostics, however this is not mandatory.

#### Why Yet Another Parser Generator?
* **Error Resilience:** The generated parser may provide similar [error resilience](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html#Why-Resilience-is-Needed) as handwritten parsers.
* **Lossless Syntax Tree:** Language tooling such as language servers or formatters require all the information about the source code including whitespaces and comments.
* **Language Server:** Get instant feedback when your grammar contains conflicts or errors.
* **Easy to Debug:** The generated parser is easy to understand and can be debugged with standard tools, as the code is not generated by a procedural macro.

#### Why LL(1) and not a more general CFL or PEG parser?
* **Error Resilience:** It seems to be the case that LL parsers are better suited than LR parsers for generating meaningful syntax trees from incomplete source code.
* **Runtime Complexity:** More general parsers such as GLR/GLL or ALL(*) can have a runtime complexity of $O(n^3)$ or $O(n^4)$ respectively for certain grammars. With LL(1) parsers you are guaranteed to have linear runtime complexity as long as your semantic actions and predicates have a constant runtime complexity.
* **Ambiguity:** The decision problem of whether an arbitrary context free grammar is ambiguous is undecidable. Warnings of a general parser generator therefore may contain false positives. In the worst case ambiguities may be found at runtime.
The PEG formalism just defines ambiguity away, which may cause the parser to parse a different language than you think.

## Error Resilience
The following example shows the difference between [Lelwel](https://0x2a-42.github.io/playground.html) and [Tree-sitter](https://tree-sitter.github.io/tree-sitter/7-playground.html), a GLR parser generator with sophisticated error recovery, when parsing certain incomplete C source code.

```c
void f() {
  g(1,
  int x = 2 +
}
```
<details>
<summary>Lelwel syntax tree</summary>

```
translation_unit [0..33]
    function_definition [0..33]
        declaration_specifiers [0..4]
            type_specifier [0..4]
                Void "void" [0..4]
                Whitespace " " [4..5]
        declarator [5..8]
            function_declarator [5..8]
                ident_declarator [5..6]
                    Identifier "f" [5..6]
                LPar "(" [6..7]
                RPar ")" [7..8]
                Whitespace " " [8..9]
        compound_statement [9..33]
            LBrace "{" [9..10]
            Whitespace "\n  " [10..13]
            expression_statement [13..17]
                call_expr [13..17]
                    ident_expr [13..14]
                        Identifier "g" [13..14]
                    LPar "(" [14..15]
                    argument_expression_list [15..17]
                        int_expr [15..16]
                            IntConst "1" [15..16]
                        Comma "," [16..17]
                        Whitespace "\n  " [17..20]
            declaration [20..31]
                declaration_specifiers [20..23]
                    type_specifier [20..23]
                        Int "int" [20..23]
                        Whitespace " " [23..24]
                init_declarator_list [24..31]
                    init_declarator [24..31]
                        declarator [24..25]
                            ident_declarator [24..25]
                                Identifier "x" [24..25]
                                Whitespace " " [25..26]
                        Assign "=" [26..27]
                        Whitespace " " [27..28]
                        initializer [28..31]
                            bin_expr [28..31]
                                int_expr [28..29]
                                    IntConst "2" [28..29]
                                    Whitespace " " [29..30]
                                Plus "+" [30..31]
                                Whitespace "\n" [31..32]
            RBrace "}" [32..33]
```
</details>

<details>
<summary>Tree-sitter syntax tree</summary>

```
translation_unit [0, 0] - [4, 0]
  function_definition [0, 0] - [3, 1]
    type: primitive_type [0, 0] - [0, 4]
    declarator: function_declarator [0, 5] - [0, 8]
      declarator: identifier [0, 5] - [0, 6]
      parameters: parameter_list [0, 6] - [0, 8]
        ( [0, 6] - [0, 7]
        ) [0, 7] - [0, 8]
    body: compound_statement [0, 9] - [3, 1]
      { [0, 9] - [0, 10]
      ERROR [1, 2] - [2, 11]
        identifier [1, 2] - [1, 3]
        ( [1, 3] - [1, 4]
        number_literal [1, 4] - [1, 5]
        , [1, 5] - [1, 6]
        assignment_expression [2, 2] - [2, 11]
          left: identifier [2, 2] - [2, 5]
          ERROR [2, 6] - [2, 7]
            identifier [2, 6] - [2, 7]
          operator: = [2, 8] - [2, 9]
          right: number_literal [2, 10] - [2, 11]
      ERROR [2, 12] - [2, 13]
        + [2, 12] - [2, 13]
      } [3, 0] - [3, 1]
```
</details>

> [!NOTE]
> Tree-sitter is an excellent tool for its intended purpose, which is mainly syntax highlighting.
> This comparison only demonstrates the possible syntax tree quality of an error resilient parser.

## Grammar Examples
The [parser for lelwel grammar files](src/frontend/lelwel.llw) (\*.llw) is itself generated by Lelwel.
There are also examples for [C without a preprocessor](examples/c/src/c.llw) (actually resolves ambiguity with semantic context information, unlike examples for ANTLR4 and Tree-sitter),  [Lua](examples/lua/src/lua.llw), [arithmetic expressions](examples/calc/src/calc.llw), [JSON](examples/json/src/json.llw), and [Oberon-0](examples/oberon0/src/oberon0.llw).

You can try out examples in the [Lelwel Playground](https://0x2a-42.github.io/playground.html).

The [following example](examples/l) shows a grammar for the toy language "L" introduced by the [Resilient LL Parsing Tutorial](https://matklad.github.io/2023/05/21/resilient-ll-parsing-tutorial.html#Introducing-L).

```antlr
token Fn='fn' Let='let' Return='return' True='true' False='false';
token Arrow='->' LPar='(' RPar=')' Comma=',' Colon=':' LBrace='{' RBrace='}'
      Semi=';' Asn='=' Plus='+' Minus='-' Star='*' Slash='/';
token Name='<name>' Int='<int>';
token Whitespace;

skip Whitespace;

start file;

file: fn*;
fn: 'fn' Name param_list ['->' type_expr] block;
param_list: '(' [param (?1 ',' param)* [',']] ')';
param: Name ':' type_expr;
type_expr: Name;
block: '{' stmt* '}';
stmt^:
  stmt_expr
| stmt_let
| stmt_return
;
stmt_expr: expr ';';
stmt_let: 'let' Name '=' expr ';';
stmt_return: 'return' [expr] ';';
expr:
  expr ('*' | '/') expr @expr_binary
| expr ('+' | '-') expr @expr_binary
| expr arg_list @expr_call
| Name @expr_name
| '(' expr ')' @expr_paren
| expr_literal ^
;
expr_literal: Int | 'true' | 'false';
arg_list: '(' [expr (?1 ',' expr)* [',']] ')';
```

## Quickstart
1. Write a grammar file and place it in the `src` directory of your crate.
   Optionally you can install the CLI or language server to validate your grammar file: `cargo install --features=cli,lsp lelwel`.
1. Add the following to your `Cargo.toml` and  `build.rs` files.
   ```toml
   [dependencies]
   logos = "0.15"
   codespan-reporting = "0.11"

   [build-dependencies]
   lelwel = "0.7"
   ```
   ```rust
   fn main() {
      lelwel::build("src/your_grammar.llw");
   }
   ```
1. Start a build. This will create a `lexer.rs` and a `parser.rs` file next to your grammar file.
   The `lexer.rs` and `parser.rs` files are supposed to be manually edited to implement the lexer and the parser callbacks. The actual parser `generated.rs` is included in `parser.rs` and written to the Cargo `OUT_DIR`.
   If you change the grammar after the `lexer.rs` and `parser.rs` files have been generated, it may be required to manually update the `Token` enum or the `ParserCallbacks` implementation.
1. Use the `lexer` and `parser` modules with the following minimal `main.rs` file for printing the CST and diagnostics.
   ```rust
   mod lexer;
   mod parser;

   use codespan_reporting::diagnostic::Severity;
   use codespan_reporting::files::SimpleFile;
   use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
   use codespan_reporting::term::{self, Config};
   use parser::*;

   fn main() -> std::io::Result<()> {
       let args: Vec<String> = std::env::args().collect();
       if args.len() != 2 {
           std::process::exit(1);
       }

       let source = std::fs::read_to_string(&args[1])?;
       let mut diags = vec![];
       let cst = Parser::parse(&source, &mut diags);
       println!("{cst}");

       let file = SimpleFile::new(&args[1], &source);
       let writer = StandardStream::stderr(ColorChoice::Auto);
       let config = Config::default();
       for diag in diags.iter() {
           term::emit(&mut writer.lock(), &config, &file, diag).unwrap();
       }
       Ok(())
   }
   ```

## Grammar Specification

Lelwel grammars are based on the formalism of [context free grammars (CFG)](https://en.wikipedia.org/wiki/Context-free_grammar) and more specifically [LL(1) grammars](https://en.wikipedia.org/wiki/LL_grammar).
There are certain extensions to the classical grammar syntax such as constructs similar to those from EBNF.

A grammar file consists of top level definitions which are independent of their order.

### Comments

C-style and C++-style comments can be used.

> **Example**
> ```antlr
> // this is a single-line comment
> /*
> this is a
> multi-line comment
> */
> ```

Documentation comments can be used before top level definitions.
> **Example**
> ```antlr
> /// this is a doc-comment
> token A B C;
> ```

> [!TIP]
> Documentation comments are shown by the language server on hover.

### Token List
A token list definition introduces a list of tokens (terminals) to the grammar.
It starts with the `token` keyword, ends with a `;` and contains a list of token names and corresponding token symbols.

A token name must start with a capital letter.
The token symbol is optional and delimited by single quotation marks.
In a regex a token can be referenced by its name or symbol.

> [!TIP]
> The token symbol is used in error messages and the generator of the `parser.rs` file.
> If the token symbol string starts with `<` and ends with `>`, the token is interpreted as a class of tokens for which the symbol is only a description.
> This influences how error messages and lexer rules are generated by default in `parser.rs`.

> **Example**
> ```antlr
> token MyKeyword='my_keyword' Int='<integer literal>' True='true' False='false';
> ```

### Skip
A `skip` definition allows to specify a list of tokens, which are ignored by the parser.
These tokens will however still be part of the syntax tree.
> **Example**
> ```antlr
> skip Whitespace Comment;
> ```

### Right
A `right` definition allows to specify a list of tokens, which are handled as right associative operators in left recursive rules.
> **Example**
> ```antlr
> right '^' '=';
> ```

### Start
A `start` definition specifies the start rule of the grammar.
There must be exactly one start definition in a grammar.
The start rule must not be referenced in a regex.

> **Example**
> ```antlr
> start translation_unit;
> ```

### Rule
A grammar rule must start with a lower case letter.
A regular expression is used to specify the right hand side of the rule.

> **Example**
> ```antlr
> translation_unit: declaration*;
> ```

#### Regular Expressions
Regular expressions are built from the following syntactic constructs.
- **Grouping**: `(...)`
- **Identifier**: `rule_name` or `TokenName`
- **Symbol**: `'token symbol'`
- **Concatenation**: `A B` which is `A` followed by `B`
- **Alternation**: `A | B` which is either `A` or `B`
- **Ordered Choice**: `A / B` which is `A` or else `B`
- **Optional**: `[A]` which is either `A` or nothing
- **Star Repetition**: `A*` which is a repetition of 0 or more `A`
- **Plus Repetition**: `A+` which is a repetition of 1 or more `A`
- **Semantic Predicate**: `?1` which is the semantic predicate number 1
- **Semantic Action**: `#1` which is the semantic action number 1
- **Semantic Assertion**: `!1` which is the semantic assertion number 1
- **Node Rename**: `@new_node_name` renames the rule syntax tree node
- **Node Elision**: `^` prevents creation of the rule syntax tree node
- **Node Marker**: `<1` marker with index 1 for a new node
- **Node Creation**: `1>new_node_name` insert node at position of marker with index 1
- **Commit**: `~` commits to a parse in an ordered choice

#### Ordered Choice
The ordered choice operator `/` has a precedence between alternation and concatenation. Unlike in the PEG formalism, the operator is restricted, such that only one ordered choice can be active at a time.

> [!WARNING]
> This restriction avoids the worst case exponential time complexity of recursive descent PEG implementations without the use of memoization.
> Due to the unlimited lookahead it is still possible to construct parsers with $O(n^2)$ worst case performance.

> [!WARNING]
> It is possible that an earlier branch prevents a later branch from ever being reached.
> Only use ordered choice as an instrument to explicitly resolve an ambiguity.

The commit operator `~` commits a parse to a choice, so in case of a failure there will be no backtracking.

> [!NOTE]
> This is useful as an optimization and to improve error reporting.
> Furthermore it can be used to avoid the ordered choice restriction in some cases, as in a concatination no ordered choice is active after the commit operator.

> **Example**
> ```antlr
> token Id Num Eq='=' Semi=';' LPar='(' RPar=')';
> start stmt;
> 
> stmt^:
>   decl_stmt
> / expr_stmt
> ;
> decl_stmt: type Id ~ ['=' expr] ';';
> expr_stmt: expr ';';
> expr:
>   Id
> | Num
> | '(' (
>     type ')' !1 ~ expr
>   / expr ')'
>   )
> ;
> type: Id;
> ```

#### Direct Left Recursion
Rules with direct left recursion are parsed using a Pratt parser.
The order of recursive rule branches in the top level alternation defines the binding power of the operator tokens.
The binding power decreases from the first to the last branch.
In a left recursive branch the follow set of the first concatenation element defines the operator tokens.
Otherwise in a right recursive branch the first set of the first concatenation element defines the operator tokens.

> [!TIP]
> Use a Pratt parser for expressions, as it improves readability and efficiency compared to an encoding of operator precedence in grammar rules.

> **Example**
> ```antlr
> expr:
>   expr '^' expr
> | ('-' | '+') expr
> | expr ('*' | '/') expr
> | expr ('+' | '-') expr
> | Num
> | '(' expr ')'
> ;
> ```

Mixing left and right associative operators in the same branch is not allowed.

> **Example**
> ```antlr
> right '=';
>
> expr:
>   expr ('+' | '=') expr // ❌ error
> | Num
> ;
> ```

#### Semantic Actions, Predicates, and Assertions
Semantic actions can be defined at any point in a regex.

Semantic predicates can be defined at the beginning of an alternation branch, an optional or a repetition. The syntax `?t` can be used to define a constant `true` predicate, which is useful for disambiguation, where one parse is always prioritized (e.g. [dangling else problem](https://en.wikipedia.org/wiki/Dangling_else)).

Semantic assertions can be placed at any position in a regex. An assertion fails if it does not return `None`. In the context of an ordered choice this can be used to steer the parser depending on semantic or syntactic information. Otherwise its diagnostic will just be emitted and the parser continues.

When the semantic action, predicate, or assertion is visited in a parse it will execute the rust code of the corresponding associated function in the `ParserCallbacks` trait implementation of the `Parser` type.

The index of actions, predicates, and assertions can be used multiple times in a rule.

> [!TIP]
> If the `ParserCallbacks` trait is implemented in the `parser.rs` file next to the grammar file (where it is generated by default), you can use the language server to jump from the grammar to the rust code.

> **Example**
> ```antlr
> token A B C D;
> 
> foo:
>   ?1 A #1 (?2 B C)* B #2
> | A !1 C #2
> / A D
> ;
> ```
> ```rust
> impl ParserCallbacks for Parser<'_> {
>     // ...
>     fn predicate_foo_1(&self) -> bool {
>         self.peek(1) == Token::B
>     }
>     fn predicate_foo_2(&self) -> bool {
>         self.context.some_condition
>     }
>     fn action_foo_1(&mut self, diags: &mut Vec<Diagnostic>) {
>         println!("executed action 1");
>     }
>     fn action_foo_2(&mut self, diags: &mut Vec<Diagnostic>) {
>         println!("executed action 2");
>     }
>     fn assertion_foo_1(&self) -> Option<Diagnostic> {
>         None
>     }
> }
> ```

#### Node Rename
The syntax tree node for a rule can be renamed when a node rename operator is visited during a parse.
> **Example**
> ```antlr
> expr:
>   Int @int_expr
> | '(' expr ')' @paren_expr
> ;
> ```
> May result in the following syntax tree.
> ```
> paren_expr
> ├─ '('
> ├─ int_expr
> │  └─ Int
> └─ ')'
> ```


#### Node Elision
Creation of a syntax tree node can be prevented if a node elision operator is visited during a parse.
> **Example**
> ```antlr
> foo: A bar bar;
> bar: B ^ | C
> ```
> May result in the following syntax tree.
> ```
> foo
> ├─ A
> ├─ B
> └─ bar
>    └─ C
> ```

Unconditional node elision can also be achieved with a special syntax by writing the operator directly after the rule name.

> [!TIP]
> This is useful for rules with a top level alternation, as it avoids an extra nesting with parentheses.

> **Example**
> ```antlr
> foo: A bar bar;
> bar^: B | C
> ```
> May result in the following syntax tree.
> ```
> foo
> ├─ A
> ├─ B
> └─ C
> ```

Node elision is not allowed in left recursive branches of a rule.
> **Example**
> ```antlr
> foo:
>   foo A ^ // ❌ error
> | B ^     // ✅ ok
> ;
> ```

#### Node Marker and Creation
A node marker can be defined in a regex, which marks the position where a syntax tree node can be inserted during a parse.
The index of such a node marker must be unique for each rule.

A node creation can be used with a corresponding node marker, if it is placed in a position where the node marker will be visited before the node creation during a parse.

> [!WARNING]
> The current implementation uses insertion into a vector for node creation, which may impact performance, if many rules/tokens are parsed between node marker and creation.
> In the worst case this may result in quadratic parse time complexity.
> However in practice it does not seem to matter too much, as the insertions mostly happen close to the end of the vector.
> Nevertheless, in the future this will probably be changed to a more efficient implementation.

> **Example**
> ```antlr
> foo: A <1 B C 1>bar;
> ```
> May result in the following syntax tree.
> ```
> foo
> ├─ A
> └─ bar
>    ├─ B
>    └─ C
> ```

The index and node name is optional for the node creation.
If the index is missing the behavior is as if a node marker at the start of the rule was used.
If the node name is missing the node name of the current rule is used.

> [!TIP]
> This is useful in combination with unconditional node elision, so a node can still be created for certain parses.

> **Example**
> ```antlr
> assign_expr^: bin_expr ['=' assign_expr >];
> ```
> This is usually preferable to
> ```antlr
> assign_expr: bin_expr ('=' assign_expr | ^);
> ```
> as the latter will not elide a node if `bin_expr` is followed by an invalid token that is not in the follow set.

A node creation with missing index is not allowed in left recursive rules.
> **Example**
> ```antlr
> foo:
>   foo A > // ❌ error
> | B >     // ❌ error
> ;
> ```

## License
Lelwel, its examples, and its generated code are licensed under either of

 * Apache License, Version 2.0
   ([LICENSE-APACHE](LICENSE-APACHE) or https://www.apache.org/licenses/LICENSE-2.0)
 * MIT license
   ([LICENSE-MIT](LICENSE-MIT) or https://opensource.org/licenses/MIT)

at your option.

## Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.
