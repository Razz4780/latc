mod asm;
mod intervals;
mod regalloc;

use crate::{context::GetSize, frontend::FunctionId, middlend::HirProgram};
use asm::{
    generator::{AsmFunction, AsmFunctionBuilder, SymbolResolver},
    Label,
};
use std::fmt::{self, Display, Formatter};

pub struct AsmProgram<'a> {
    extern_funs: Vec<Label<'a>>,
    vtables: Vec<(Label<'a>, Vec<Label<'a>>)>,
    funs: Vec<AsmFunction<'a>>,
    literals: Vec<(Label<'a>, &'a str)>,
}

impl<'a> AsmProgram<'a> {
    pub fn new(hir_prog: HirProgram<'a>) -> Self {
        let mut funs = Vec::with_capacity(hir_prog.functions().len());

        let mut symbols = SymbolResolver::new(hir_prog.context());

        for (name, fun) in hir_prog.functions() {
            let params = match name {
                FunctionId::Global { name } => hir_prog
                    .context()
                    .function(name)
                    .unwrap()
                    .args()
                    .iter()
                    .map(|a| a.ty.size())
                    .collect(),
                FunctionId::Method { name, class } => hir_prog
                    .context()
                    .class(class)
                    .unwrap()
                    .method(name)
                    .unwrap()
                    .as_fun()
                    .args()
                    .iter()
                    .map(|a| a.ty.size())
                    .collect(),
            };
            let res = AsmFunctionBuilder::new(*name, params, fun, symbols).run();
            symbols = res.1;

            funs.push(res.0);
        }

        let vtables = hir_prog
            .context()
            .classes()
            .iter()
            .filter_map(|(name, class)| {
                let mut methods = class.methods().iter().collect::<Vec<_>>();
                if methods.is_empty() {
                    return None;
                }
                methods.sort_unstable_by_key(|m| m.vtable_idx());
                let methods = methods
                    .into_iter()
                    .map(|m| Label::Method(m.origin_class(), m.as_fun().name()))
                    .collect::<Vec<_>>();

                Some((Label::VTable(name), methods))
            })
            .collect::<Vec<_>>();

        Self {
            extern_funs: vec![
                Label::Lib("newArray"),
                Label::Lib("newObject"),
                Label::Lib("addStrings"),
                Label::Lib("cmpStrings"),
                Label::Function("error"),
                Label::Function("printInt"),
                Label::Function("printString"),
                Label::Function("readInt"),
                Label::Function("readString"),
            ],
            vtables,
            funs,
            literals: symbols.literals(),
        }
    }
}

impl<'a> Display for AsmProgram<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for fun in &self.extern_funs {
            writeln!(f, "extern {}", fun)?;
        }
        writeln!(f)?;

        writeln!(f, "global main")?;
        writeln!(f)?;

        writeln!(f, "section .rodata")?;
        writeln!(f)?;

        for (label, lit) in &self.literals {
            let lit_string = lit
                .as_bytes()
                .iter()
                .map(|b| format!("0x{:x}", b))
                .collect::<Vec<_>>()
                .join(",");
            writeln!(f, "{}: db {},0", label, lit_string)?;
        }
        writeln!(f)?;

        for (label, methods) in &self.vtables {
            let meth_string = methods
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",");
            writeln!(f, "{}: dq {}", label, meth_string)?;
        }
        writeln!(f)?;

        writeln!(f, "section .text")?;
        writeln!(f)?;

        for fun in &self.funs {
            writeln!(f, "{}", fun)?;
        }

        writeln!(f, "main:")?;
        writeln!(f, "\tpush rbp")?;
        writeln!(f, "\tmov rbp, rsp")?;
        writeln!(f, "\tcall {}", Label::Function("main"))?;
        writeln!(f, "\tpop rbp")?;
        writeln!(f, "\tret")?;

        Ok(())
    }
}
