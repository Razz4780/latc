.PHONY: clean

CC = gcc
CFLAGS = -Wall -Wextra -std=c17 -O2

all: runtime compiler

runtime:
	$(CC) -c $(CFLAGS) lib/runtime.c -o lib/runtime.o

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
