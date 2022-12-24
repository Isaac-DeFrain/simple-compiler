all: clean build test

build:
			@dune build

clean:
			@dune clean

help:
			@dune exec -- _build/default/bin/main.exe -h

interpreter:
			@dune exec -- _build/default/bin/main.exe -i

test: clean
			@dune runtest
