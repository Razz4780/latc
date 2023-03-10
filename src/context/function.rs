use crate::{
    ast::{ClassDef, FnDef, Type},
    error::{self, StaticCheckError},
};

#[derive(Debug, Clone)]
pub struct Argument<'a> {
    pub name: &'a str,
    pub ty: Type<'a>,
}

#[derive(Debug, Clone)]
pub struct Function<'a> {
    ret: Option<Type<'a>>,
    args: Vec<Argument<'a>>,
    name: &'a str,
}

impl<'a> Function<'a> {
    pub fn new(
        def: &FnDef<'a>,
        method_of: Option<&ClassDef<'a>>,
    ) -> Result<Self, StaticCheckError> {
        let dup = super::find_duplicate(def.args.iter().map(|a| a.name));
        if let Some(dup) = dup {
            error::bail!(
                dup.offset,
                "argument \"{}\" already defined in function \"{}\"",
                dup.inner,
                def.ident.inner,
            );
        }

        let mut args = Vec::with_capacity(def.args.len() + usize::from(method_of.is_some()));
        if let Some(class) = method_of {
            args.push(Argument {
                name: "self",
                ty: Type::class(class.ident.inner),
            })
        }
        for arg in &def.args {
            args.push(Argument {
                name: arg.name.inner,
                ty: arg.arg_type.inner,
            });
        }

        Ok(Self {
            ret: def.ret.map(|l| l.inner),
            args,
            name: def.ident.inner,
        })
    }

    pub fn ret(&self) -> Option<&Type<'a>> {
        self.ret.as_ref()
    }

    pub fn args(&self) -> &[Argument<'a>] {
        self.args.as_slice()
    }

    pub fn name(&self) -> &'a str {
        self.name
    }
}

pub fn public_builtins() -> Vec<Function<'static>> {
    vec![
        Function {
            ret: None,
            args: vec![Argument {
                name: "",
                ty: Type::INT,
            }],
            name: "printInt",
        },
        Function {
            ret: None,
            args: vec![Argument {
                name: "",
                ty: Type::STR,
            }],
            name: "printString",
        },
        Function {
            ret: None,
            args: vec![],
            name: "error",
        },
        Function {
            ret: Some(Type::INT),
            args: vec![],
            name: "readInt",
        },
        Function {
            ret: Some(Type::STR),
            args: vec![],
            name: "readString",
        },
    ]
}
