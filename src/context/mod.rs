mod builder;
mod class;
mod function;
mod layout;

use crate::{
    ast::{ClassDef, Def, Leaf, SimpleType, Type},
    error::{self, StaticCheckError},
};
use builder::ContextBuilder;
use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Formatter},
    rc::Rc,
};

pub use class::{Class, Method};
pub use function::{Argument, Function};

/// Size in bytes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Bytes {
    B8,
    B4,
    B1,
}

impl Debug for Bytes {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Self::B8 => "i64",
            Self::B4 => "i32",
            Self::B1 => "i8",
        };

        f.write_str(as_str)
    }
}

impl From<Bytes> for i32 {
    fn from(value: Bytes) -> Self {
        match value {
            Bytes::B8 => 8,
            Bytes::B4 => 4,
            Bytes::B1 => 1,
        }
    }
}

pub trait GetSize {
    fn size(&self) -> Bytes;
}

impl<'a> GetSize for Type<'a> {
    fn size(&self) -> Bytes {
        match self {
            Self::Arr(..) => Bytes::B8,
            Self::Simple(s) => s.size(),
        }
    }
}

impl<'a> GetSize for SimpleType<'a> {
    fn size(&self) -> Bytes {
        match self {
            SimpleType::Class(..) => Bytes::B8,
            SimpleType::Str => Bytes::B8,
            SimpleType::Int => Bytes::B4,
            SimpleType::Bool => Bytes::B1,
        }
    }
}

/// Finds the first duplicate in the given identifiers.
fn find_duplicate<'a, I: Iterator<Item = Leaf<&'a str>>>(mut idents: I) -> Option<Leaf<&'a str>> {
    let mut seen = HashSet::new();
    idents.find(|ident| !seen.insert(ident.inner))
}

/// Context of a program.
/// Contains function and class signatures.
#[derive(Debug)]
pub struct Context<'a> {
    functions: HashMap<&'a str, Function<'a>>,
    classes: HashMap<&'a str, Rc<Class<'a>>>,
}

impl<'a> Context<'a> {
    /// Byte offset of the array elements pointer inside an array struct.
    pub const ARRAY_ELEMS_OFFSET: i32 = 0;
    /// Byte offset of the array length inside an array struct.
    pub const ARRAY_LENGTH_OFFSET: i32 = 8;
    /// Byte offset of the vtable pointer inside a class.
    pub const CLASS_VTABLE_OFFSET: i32 = 0;

    /// Statically checks the given [`Def`]s and creates a new instance of this struct.
    pub fn new(defs: &[Def<'a>]) -> Result<Self, StaticCheckError> {
        let mut builder = ContextBuilder::default();

        let mut todos_by_parent: HashMap<&'a str, Vec<&ClassDef<'a>>> = Default::default();
        let mut work_queue: Vec<&ClassDef<'a>> = Default::default();

        for def in defs {
            match def {
                Def::Class(class_def) => match class_def.parent {
                    Some(parent) => todos_by_parent
                        .entry(parent.inner)
                        .or_default()
                        .push(class_def),
                    None => work_queue.push(class_def),
                },
                Def::Fn(fn_def) => builder.add_function(fn_def)?,
            }
        }

        while let Some(class_def) = work_queue.pop() {
            builder.add_class(class_def)?;
            work_queue.extend(
                todos_by_parent
                    .remove(class_def.ident.inner)
                    .unwrap_or_default(),
            );
        }

        if let Some(class) = todos_by_parent.into_values().flatten().next() {
            let parent = class.parent.unwrap();
            error::bail!(parent.offset, "unknown parent class \"{}\"", parent.inner);
        }

        builder.build()
    }

    pub fn functions(&self) -> &HashMap<&'a str, Function<'a>> {
        &self.functions
    }

    pub fn function(&self, name: &str) -> Option<&Function<'a>> {
        self.functions.get(name)
    }

    pub fn classes(&self) -> &HashMap<&'a str, Rc<Class<'a>>> {
        &self.classes
    }

    pub fn class(&self, name: &str) -> Option<&Class<'a>> {
        self.classes.get(name).map(Rc::as_ref)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Arg, Def, FnDef, Leaf, Type},
        context::builder::ContextBuilder,
    };

    fn make_ctx<'a>(defs: &[Def<'a>]) -> Result<Context<'a>, StaticCheckError> {
        let mut builder = ContextBuilder::default();
        for def in defs {
            match def {
                Def::Fn(fun) => builder.add_function(fun)?,
                Def::Class(class) => builder.add_class(class)?,
            }
        }
        builder.build()
    }

    #[test]
    fn no_main() {
        make_ctx(&[]).expect_err("no main function should fail");
    }

    #[test]
    fn invalid_main() {
        make_ctx(&[Def::Fn(FnDef {
            ret: None,
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect_err("main returning no value should fail");

        make_ctx(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::BOOL, 0)),
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect_err("main returning a boolean should fail");

        make_ctx(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::INT, 0)),
            ident: Leaf::new("main", 0),
            args: vec![Arg {
                arg_type: Leaf::new(Type::INT, 0),
                name: Leaf::new("arg", 0),
            }],
            stmts: vec![],
        })])
        .expect_err("main with arguments should fail");
    }

    #[test]
    fn only_valid_main() {
        make_ctx(&[Def::Fn(FnDef {
            ret: Some(Leaf::new(Type::INT, 0)),
            ident: Leaf::new("main", 0),
            args: vec![],
            stmts: vec![],
        })])
        .expect("only valid main function should not fail");
    }

    #[test]
    fn arg_duplicate() {
        Function::new(
            &FnDef {
                ret: None,
                ident: Leaf::new("fun", 0),
                args: vec![
                    Arg {
                        arg_type: Leaf::new(Type::INT, 0),
                        name: Leaf::new("arg1", 0),
                    },
                    Arg {
                        arg_type: Leaf::new(Type::BOOL, 0),
                        name: Leaf::new("arg1", 0),
                    },
                ],
                stmts: vec![],
            },
            None,
        )
        .expect_err("duplicate function argument should fail");
    }
}
