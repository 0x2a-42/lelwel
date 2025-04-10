# lelwel-c
A parser for C11 with some GNU extensions and without the preprocessor. Backslashes followed by newline characters are currently only skipped when they are part of whitespaces or comments.

Scopes and typedef names are tracked during parsing, which is required to resolve some ambiguities in the grammar. Otherwise no semantic checks are done.

To parse most programs, you first have to create the preprocessed output by running `gcc -E -P` and remove all remaining `#pragma` directives.

The parser is capable of parsing all examples (`bzip2.c`, `gzip.c`, `oggenc.c`, `gcc.c`) from https://people.csail.mit.edu/smcc/projects/single-file-programs after preprocessing. On an Intel Core i7-8550U CPU the preprocessed `gcc.c` file with 473154 lines is parsed in 273 ms when the CST is not being printed. This translates to about 1.7 million lines of code per second.
