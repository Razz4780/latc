# What for?
For compiling Latte programs to x64.

# Why?
The compiler construction course at MIMUW.

# But what is Latte?
A simple Java-like object-oriented language. Some of its features:
1. 32-bit signed integeres, booleans, on-heap strings, arrays and user-defined classes.
2. Virtual methods.
3. Very simple IO through builtin `readInt`, `readString`, `printInt`, `printString` methods.
For some examples of correct Latte programs, see `test_inputs/good` directory.

# Technicalities
This compiler is written in Rust programming language. It uses [LALRPOP](https://github.com/lalrpop/lalrpop) for generating a parser. The file `src/grammar.lalrpop` contains the grammar used by `LALRPOP` to generate Rust code during compilation (see `build.rs`).

Implemented code optimizations:
1. Const propagation
2. Dead code removal
3. GCSE
4. Linear register allocation

Compilation:
1. The input program is parsed into an AST using the parser generated with `LALRPOP`
2. The AST is statically checked and simplified (e.g. `for` loops are transformed into `while` loops with anonymous variables) - `src/frontend` module
3. Each function definition is transformed into a high-level intermediate SSA representation modeled after LLVM - `src/middlend` module
4. Intermediate representation is transformed into x64 assembly in nasm format - `src/backend` module
5. x64 assembly is processed using `nasm` executable and linked with Latte runtime with `gcc`

# Building
Command `make` produces compilation artifacts in both `target` and `runtime` directories. To compile the project, `make` uses `gcc` and `cargo` - the standard package manager for Rust.

Apart from building the compiler, `make` also builds the Latte runtime. The source code for that is located in `lib/runtime.c`.

Running the compiler requires compiled Latte runtime, `gcc` and `nasm`. For more info, see `cargo run -- --help`.
