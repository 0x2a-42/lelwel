# lelwel-json
This project is a simple example for the lelwel parser generator.

The program reads a JSON file and returns 0 if it is valid or 1 otherwise.
It passes all the parsing tests from [https://github.com/nst/JSONTestSuite](https://github.com/nst/JSONTestSuite).

## Example

```sh
$ cargo run some_file.json
$ echo $?
0
```
