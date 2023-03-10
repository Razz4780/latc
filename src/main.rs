#[macro_use]
extern crate lalrpop_util;

lalrpop_mod!(#[allow(clippy::all)] pub grammar);

use anyhow::{Context, Result};
use clap::Parser;
use grammar::ProgramParser;
use latc::{
    backend::AsmProgram,
    error::{ErrorContext, StaticCheckErrorDisplay},
    frontend::CheckedProgram,
    middlend::HirProgram,
};
use std::{
    fs::{self, File},
    io::Write,
    path::PathBuf,
    process::Command,
    process::{ExitCode, Stdio},
};

#[derive(Parser)]
struct Arguments {
    /// Stop after validating input program.
    #[clap(short, long, default_value_t = false)]
    check: bool,
    /// "nasm" executable location.
    #[clap(short, long, default_value = "nasm")]
    nasm: String,
    /// "gcc" executable location.
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
    let has_lat_ext = args
        .file
        .extension()
        .map(|ext| ext == "lat")
        .unwrap_or_default();
    anyhow::ensure!(has_lat_ext, "input file must have a \".lat\" extension");

    let input = fs::read_to_string(&args.file).context("failed to read input file")?;
    let checked_program = check(&input).context("static check failed")?;

    if args.check {
        return Ok(());
    }

    let hir_program = HirProgram::new(checked_program);
    let asm_program = AsmProgram::new(hir_program);

    let latc_output = args.file.as_path().with_extension("s");
    let mut file = File::create(&latc_output)
        .with_context(|| format!("failed to create output file at {}", latc_output.display()))?;
    write!(file, "{}", asm_program)
        .with_context(|| format!("failed to write output file at {}", latc_output.display()))?;

    let nasm_output = args.file.as_path().with_extension("o");
    let nasm = Command::new(args.nasm)
        .arg("-f")
        .arg("elf64")
        .arg(latc_output)
        .arg("-o")
        .arg(&nasm_output)
        .stderr(Stdio::null())
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .status()
        .context("failed to execute \"nasm\" command")?;
    anyhow::ensure!(nasm.success(), "\"nasm\" command failed");

    let gcc_output = args.file.as_path().with_extension("out");
    let gcc = Command::new(args.gcc)
        .arg("-no-pie")
        .arg(nasm_output)
        .arg(args.runtime)
        .arg("-o")
        .arg(gcc_output)
        .stderr(Stdio::null())
        .stdout(Stdio::null())
        .stdin(Stdio::null())
        .status()
        .context("failed to execute \"gcc\" command")?;
    anyhow::ensure!(gcc.success(), "\"gcc\" command failed");

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
