#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

use anyhow::{Context, Result};
use clap::Parser;
use grammar::ProgramParser;
use latc::{
    error::{ErrorContext, StaticCheckErrorDisplay},
    frontend::CheckedProgram,
    middlend::HirProgram,
};
use std::{fs, path::PathBuf, process::ExitCode};

#[derive(Parser)]
struct Arguments {
    /// Stop after validating input program.
    #[clap(short, long, default_value_t = false)]
    check: bool,
    /// 'nasm' executable location.
    #[clap(short, long, default_value = "nasm")]
    nasm: String,
    /// 'gcc' executable location.
    #[clap(short, long, default_value = "gcc")]
    gcc: String,
    /// Latte runtime location.
    #[clap(short, long, default_value = "lib/runtime.o")]
    runtime: String,
    /// Input Latte program location.
    file: PathBuf,
}

fn check(input: &str) -> Result<CheckedProgram<'_>, StaticCheckErrorDisplay> {
    let ctx = ErrorContext::new(input);
    let defs = ProgramParser::new()
        .parse(input)
        .map_err(|e| ctx.display(e.into()))?;
    CheckedProgram::new(defs).map_err(|e| ctx.display(e))
}

fn run(args: Arguments) -> Result<()> {
    let input = fs::read_to_string(&args.file).context("failed to read input file")?;
    let checked_program = check(&input).context("static check failed")?;

    if args.check {
        return Ok(());
    }

    let _ = HirProgram::new(checked_program);

    Ok(())
}

fn main() -> ExitCode {
    let args = Arguments::parse();
    match run(args) {
        Ok(()) => {
            eprintln!("OK");
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("ERROR");
            eprintln!("{:?}", e);
            ExitCode::FAILURE
        }
    }
}
