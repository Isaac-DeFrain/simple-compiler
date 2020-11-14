build:
			dune build

clean:
			dune clean

help:
			dune exec -- ./src/main.exe -h

interpreter:
			dune exec -- ./src/main.exe -i

test:
			dune runtest test

.PHONY: build clean help interpreter test
