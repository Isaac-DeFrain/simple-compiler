(lang dune 3.3)

(using menhir 2.1)

(name simple_compiler)

(generate_opam_files true)

(source
 (github Isaac-DeFrain/simple-compiler))

(authors "Isaac DeFrain")

(maintainers "Isaac DeFrain")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name simple_compiler)
 (synopsis "A simple compiler for the tinyL language")
 (description "Lexer, parser, compiler, interpreter, and code optimizer")
 (depends ocaml ocamlformat dune core sexplib menhir)
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
