.PHONY: clean

all: runtime compiler

runtime:
	gcc -c lib/runtime.c -o lib/runtime.o

compiler:
	cargo build --release

clean:
	rm -rf target lib/runtime.o
