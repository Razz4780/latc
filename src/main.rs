#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

use grammar::ProgramParser;
use latc::{
    error::{ErrorContext, LatteError},
    frontend::CheckedProgram,
};
use std::{
    io::{self, Read},
    process::ExitCode,
};

fn process(input: &str) -> Result<CheckedProgram<'_>, LatteError> {
    let defs = ProgramParser::new().parse(input)?;
    let checked_program = CheckedProgram::new(defs)?;

    Ok(checked_program)
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
        Ok(..) => {
            // println!("{}", prog);

            eprintln!("OK");

            ExitCode::SUCCESS
        }
        Err(e) => {
            let ctx = ErrorContext::new(&input);

            eprintln!("ERROR");
            eprintln!("{}", ctx.display(&e));

            ExitCode::FAILURE
        }
    }
}
