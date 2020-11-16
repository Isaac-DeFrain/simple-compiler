all: build test

build:
			dune build

clean:
			dune clean

help:
			dune exec -- ./src/main.exe -h

interpreter:
			dune exec -- ./src/main.exe -i

test:
			dune runtest

.PHONY: all build clean help interpreter test
