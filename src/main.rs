#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

use grammar::ProgramParser;
use latc::{ast::Lines, backend, error::LatteError, frontend, middlend};
use std::{
    io::{self, Read},
    process::ExitCode,
};

fn process(input: &str) -> Result<(), LatteError> {
    let defs = ProgramParser::new().parse(input)?;
    let prog = frontend::process(defs)?;
    let hir_prog = middlend::process(prog);
    let asm_prog = backend::process(hir_prog);

    print!("{}", asm_prog);

    Ok(())
}

fn main() -> ExitCode {
    let mut input = String::new();
    let err = io::stdin().read_to_string(&mut input).err();
    if let Some(e) = err {
        eprintln!("ERROR");
        eprintln!("failed to read STDIN {}", e);
        return ExitCode::FAILURE;
    }

    if let Err(e) = process(&input) {
        eprintln!("ERROR");
        let lines = Lines::new(&input);
        eprintln!("{}", e.display(&lines));
        ExitCode::FAILURE
    } else {
        eprintln!("OK");
        ExitCode::SUCCESS
    }
}
