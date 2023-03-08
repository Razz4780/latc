use crate::{
    ast::{FnDef, Type},
    error::LatteError,
};
use std::collections::{HashMap, HashSet};

/// A sanitized function declaration.
/// # Invariants
/// * Argument names are unique.
/// * If this is a method, the first argument is a reference to the object.
#[derive(Debug)]
pub struct FnDecl<'a> {
    ret: Option<Type<'a>>,
    class: Option<&'a str>,
    args: Vec<(&'a str, Type<'a>)>,
}

impl<'a> FnDecl<'a> {
    pub fn is_builtin(name: &str) -> bool {
        matches!(
            name,
            "printInt" | "printString" | "error" | "readInt" | "readString"
        )
    }

    /// Returns the type of value returned by this function.
    pub fn ret(&self) -> Option<Type<'a>> {
        self.ret
    }

    /// If this function is a method, returns the name of the class it is defined in.
    pub fn class(&self) -> Option<&'a str> {
        self.class
    }

    /// Returns the arguments this function takes.
    pub fn args(&self) -> &[(&'a str, Type<'a>)] {
        &self.args
    }
    /// Tries to sanitize the given dirty definition.
    pub fn new(dirty: &FnDef<'a>, member_of: Option<&'a str>) -> Result<Self, LatteError> {
        let mut args = HashSet::new();
        for arg in &dirty.args {
            if !args.insert(arg.name.inner) {
                return Err(LatteError::new_at(
                    format!(
                        "argument \"{}\" already defined in function \"{}\"",
                        arg.name.inner, dirty.ident.inner
                    ),
                    arg.name.offset,
                ));
            }
        }

        let mut args = Vec::with_capacity(args.len() + usize::from(member_of.is_some()));
        if let Some(class) = member_of {
            args.push(("self", Type::class(class)));
        }
        for arg in &dirty.args {
            args.push((arg.name.inner, arg.arg_type.inner));
        }

        Ok(Self {
            ret: dirty.ret.map(|l| l.inner),
            class: member_of,
            args,
        })
    }

    pub fn builtins() -> HashMap<&'a str, FnDecl<'a>> {
        HashMap::from([
            (
                "printInt",
                FnDecl {
                    ret: None,
                    class: None,
                    args: vec![("", Type::INT)],
                },
            ),
            (
                "printString",
                FnDecl {
                    ret: None,
                    class: None,
                    args: vec![("", Type::STR)],
                },
            ),
            (
                "error",
                FnDecl {
                    ret: None,
                    class: None,
                    args: vec![],
                },
            ),
            (
                "readInt",
                FnDecl {
                    ret: Some(Type::INT),
                    class: None,
                    args: vec![],
                },
            ),
            (
                "readString",
                FnDecl {
                    ret: Some(Type::STR),
                    class: None,
                    args: vec![],
                },
            ),
        ])
    }
}
