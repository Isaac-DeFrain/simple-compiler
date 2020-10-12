# TinyL compiler, interpreter, and RISC code optimizer

In this project, we implement a compiler, interpreter, and code optimizer for the `tinyL` language.

The lexer and parser are implemented using the OCaml parser generator, Menhir.

## tinyL grammar

```txt
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

## Compiler

The compiler is written in 5 parts:

1. `lexer.mll` generates the lexer

2. `parser.mly` generates the parser (tinyL AST)

3. `tinyL.ml` defines the tinyL AST

4. `risc.ml` defines the RISC AST

5. `gen.ml` translates tinyL ASTs to RISC ASTs

## Interpreter

There are 2 interpreters:

1. `eval.ml` interprets tinyL ASTs

2. `eval_risc.ml` interprets RISC ASTs

## Optimizer

`optimize.ml` eliminates dead RISC code

## Command line tool

`main.ml` defines the command line functionality
