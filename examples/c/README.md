# lelwel-c
A parser for C11 with some GNU extensions and without the preprocessor. Backslashes followed by newline characters are currently only skipped when they are part of whitespaces or comments.

Scopes and typedef names are tracked during parsing, which is required to resolve some ambiguities in the grammar. Otherwise no semantic checks are done.

To parse most programs, you first have to create the preprocessed output by running `gcc -E -P` and remove all remaining `#pragma` directives.
