# Roman numeral implementation

A basic implementation of Roman numerals that came out of some random weekend coding for fun. There was no other point
than fun relaxing programming. The definition of Roman numerals is based on the Wikipedia article at
https://en.wikipedia.org/wiki/Roman_numerals.

## CLI

Basic usage:

```shell
# Print general help information
roman-numerals help
```

```shell
# Print the help info for `convert`
roman-numerals help convert
```

```shell
# Print the help info for `repl`
roman-numerals help repl
```

```shell
# Print the version
roman-numerals --version
```

### REPL (read-eval-print-loop)

Use the REPL to interactively convert one value at a time.

```shell
roman-numerals repl
```

### Convert

Use the CLI to convert a single value at a time. Some examples:

```shell
# Outputs IV
roman-numerals convert 4

# Outputs 4
roman-numerals convert IV
```
