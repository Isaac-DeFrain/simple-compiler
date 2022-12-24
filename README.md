# TinyL compiler, interpreter, and code optimizer

Implementation of compiler, interpreter, and code optimizer for the [`tinyL` language](./README.md#tinyl-grammar)

The lexer and parser are implemented with `menhir`

## Build

```sh
git clone https://github.com/Isaac-DeFrain/simple-compiler.git
cd simple-compiler
opam switch create simple-compiler 4.14.1
opam install . --deps-only
eval $(opam env)
dune build
```

### Help

```
make help
```

### Interactive mode

```sh
# translation from tinyL to risc
dune exec -- _build/default/bin/main.exe -i
# evaluate tinyL program
dune exec -- _build/default/bin/main.exe -i -e
```

## TinyL grammar

```
<program>   ::= <stmt_list> .
<stmt list> ::= <stmt> <morestmts>
<morestmts> ::= ; <stmt_list> | ε
<stmt>      ::= <assign> | <read> | <print>
<assign>    ::= <variable> = <expr>
<read>      ::= ? <variable>
<print>     ::= # <variable>
<expr>      ::= + <expr> <expr>
              | − <expr> <expr>
              | * <expr> <expr>
              | & <expr> <expr>
              | ^ <expr> <expr>
              | <variable>
              | <digit>
<variable>  ::= a | b | c | d | e
<digit>     ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
```

## Example

```sh
# tinyL program
cat test/test1.tyl 
# Output:
#   ?a;?b;c=+ab;#c.

# risc-ify the tinyL
dune exec -- _build/default/bin/main.exe -f test/test1.tyl
# Output:
#   READ a;
#   READ b;
#   c = ADD a b ;
#   PRINT c;

# now evaluate the tinyL program
dune exec -- _build/default/bin/main.exe -f test/test1.tyl -e
# we need to provide values for the variables a and b, e.g.
#   a -> 19
#   b -> 23
# Output:
#   c -> 42
```

[more examples](./test)

## Compiler

Written in 5 parts:

1. `lexer.mll` generates the lexer

2. `parser.mly` generates the parser (tinyL AST)

3. `ast.ml` defines the tinyL AST

4. `risc.ml` defines the RISC AST

5. `gen.ml` translates tinyL ASTs to RISC ASTs

## Interpreter

There are 2 interpreters:

1. `eval.ml` interprets tinyL ASTs

2. `eval_risc.ml` interprets RISC ASTs

## Code Optimizer

`optimize.ml` eliminates dead RISC code

## Command line tool

`main.ml` defines the command line functionality
