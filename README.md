# Compiler
This compiler is written in Rust programming language. It uses the [LALRPOP](https://github.com/lalrpop/lalrpop) for generating a parser. The file `src/grammar.lalrpop` contains the grammar used by the `LALRPOP` to generate code during compilation (see `build.rs`).

Command `make` produces compilation artifacts in the `target` directory in the root of the project. These artifacts are used by the `latc` and `latc_x86_64` scripts. To compile the project, `make` uses `cargo` - the standard package manager for Rust.

`latc` and `latc_x86_64` are essentialy the same script. They take one argument - path to the Latte file.

# Implemented extensions
* linear register allocation
* arrays
* objects with virtual methods
* gcse

# Dependencies
* [LALRPOP](https://github.com/lalrpop/lalrpop) - Rust package for parser generation.
* [regex](https://github.com/rust-lang/regex) - Rust package for handling regular expressions.
