#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

use grammar::ProgramParser;
use latc::{
    ast::Lines,
    backend::{self, AsmProgram},
    error::LatteError,
    frontend, middlend,
};
use std::{
    io::{self, Read},
    process::ExitCode,
};

fn process(input: &str) -> Result<AsmProgram<'_>, LatteError> {
    let defs = ProgramParser::new().parse(input)?;
    let prog = frontend::process(defs)?;
    let hir_prog = middlend::process(prog);
    let asm_prog = backend::process(hir_prog);

    Ok(asm_prog)
}

fn main() -> ExitCode {
    let mut input = String::new();
    let res = io::stdin().read_to_string(&mut input);
    if let Err(e) = res {
        eprintln!("ERROR");
        eprintln!("failed to read STDIN: {}", e);

        return ExitCode::FAILURE;
    }

    match process(&input) {
        Ok(prog) => {
            println!("{}", prog);

            eprintln!("OK");
            ExitCode::SUCCESS
        }
        Err(e) => {
            let lines = Lines::new(&input);

            eprintln!("ERROR");
            eprintln!("{}", e.display(&lines));

            ExitCode::FAILURE
        }
    }
}
