.PHONY: clean

all: runtime compiler

runtime:
	gcc -c lib/runtime.c -o lib/runtime.o

compiler:
	cargo build --release && cp target/release/latc latc && cp latc latc_x86_64

clean:
	rm -rf \
		target \
		lib/runtime.o \
		test_inputs/*/*.s \
		test_inputs/*/*.o \
		test_inputs/*/*.out \
		test_inputs/*/*.output2 \
		latc \
		latc_x86_64
