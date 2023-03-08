.PHONY: clean

all: runtime compiler

runtime:
	gcc -c lib/runtime.c -o lib/runtime.o

compiler:
	./compile_sandbox.sh && cp latc latc_x86_64

clean:
	rm -rf target .cargo_sandbox .rustup_sandbox latc_x86_64 lib/runtime.o
