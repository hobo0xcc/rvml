# rvml -- min-caml compiler with let-polymorphism

`rvml` is a min-caml compiler with let-polymorphism.
`rvml`'s polymorphic type inference is implemented in level-based algorithm discovered by Didier Rémy.

![](img/contest.png)

## Require

`llvm-13.0.1`

In macOS, you can install llvm with Homebrew.

```
$ brew install llvm
```

If you use other OS or architecture, you may need to build llvm from source. See [https://llvm.org/docs/GettingStarted.html](https://llvm.org/docs/GettingStarted.html).

## CLI

```
rvml 0.1.0
hobo0xcc
min-caml compiler with let-polymorphism

USAGE:
    rvml [FLAGS] [OPTIONS] [INPUT]

FLAGS:
    -r, --repl       REPL
    -h, --help       Prints help information
    -V, --version    Prints version information

OPTIONS:
    -o, --output <OUTPUT>    Output file
        --target <TARGET>    Specify target triple

ARGS:
    <INPUT>    Input file
```

## Build

```
$ cargo build
```

## Run

```
$ cargo run filename -o main.o
```

Compiler will generate object file that can link to generate executable file.
For example:

```
$ cargo run examples/poly1.ml -o main.o
$ gcc -o main main.o
$ ./main
$ echo $?    # This should put 42.
```

To use primitive functions, you'll need to link generated object file with `lib64/lib.c`.

```
$ gcc -c -o lib.o lib64/lib.c
$ gcc -o main main.o lib.o
```

## Examples

### let polymorphism

```ocaml
let rec f x = x in if f (f true) then f 42 else f 2
```

### fibonacci number

```ocaml
let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2) in fib 10
```

### factorial

```ocaml
let rec fact n = if n = 0 then 1 else n * fact (n - 1) in fact 5
```

### tuple

```ocaml
let rec f x = x, x, x in let (a, b, c) = f 3 in a + b + c
```
