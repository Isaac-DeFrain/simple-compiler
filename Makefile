all: clean build test

build:
			@dune build

clean:
			@dune clean

help:
			@dune exec -- ./src/main.exe -h

interpreter:
			@dune exec -- ./src/main.exe -i

test: clean
			@dune runtest
