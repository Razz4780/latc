mod builder;
mod class;
mod function;

use crate::{error::LatteError, ast::{Leaf, Type, Def, ClassDef}};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    fmt::{self, Debug, Formatter},
};
use builder::ContextBuilder;

pub use function::{Argument, Function};
pub use class::{Class,Method};


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

impl From<Bytes> for usize {
    fn from(value: Bytes) -> Self {
        match value {
            Bytes::B8 => 8,
            Bytes::B4 => 4,
            Bytes::B1 => 1,
        }
    }
}

impl<'a> Type<'a> {
    pub fn size(self) -> Bytes {
        match self {
            Self::Arr(..) | Self::STR => Bytes::B8,
            Self::INT => Bytes::B4,
            Self::BOOL => Bytes::B1,
        }
    }
}

fn find_duplicate<'a, I: Iterator<Item = Leaf<&'a str>>>(idents: I) -> Option<Leaf<&'a str>> {
    let mut seen = HashSet::new();
    for ident in idents {
        if !seen.insert(ident.inner) {
            return Some(ident);
        }
    }

    None
}

#[derive(Debug)]
pub struct Context<'a> {
    functions: HashMap<&'a str, Function<'a>>,
    classes: HashMap<&'a str, Rc<Class<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new(defs: &[Def<'a>]) -> Result<Self, LatteError> {
        let mut builder = ContextBuilder::default();

        let mut todos_by_parent: HashMap<&'a str, Vec<&ClassDef<'a>>> = Default::default();
        let mut work_queue: Vec<&ClassDef<'a>> = Default::default();

        for def in defs {
            match def {
                Def::Class(class_def) => match class_def.parent {
                    Some(parent) => todos_by_parent.entry(parent.inner).or_default().push(class_def),
                    None => work_queue.push(class_def),
                }
                Def::Fn(fn_def) => builder.add_function(&fn_def)?,
            }
        }

        while let Some(class_def) = work_queue.pop() {
            builder.add_class(&class_def)?;
            work_queue.extend(todos_by_parent.remove(class_def.ident.inner).unwrap_or_default());
        }

        builder.build()
    }

    pub fn functions(&self) -> &HashMap<&'a str, Function<'a>> {
        &self.functions
    }

    pub fn classes(&self) -> &HashMap<&'a str, Rc<Class<'a>>> {
        &self.classes
    }

    // pub fn function_context(self: Rc<Self>, name: &'a str) -> Option<Context<'a>> {
    //     if self.functions.contains_key(name) {
    //         Some(Context {
    //             declarations: self,
    //             class_name: None,
    //             name,
    //         })
    //     } else {
    //         None
    //     }
    // }

    // pub fn method_context(
    //     self: Rc<Self>,
    //     name: &'a str,
    //     class_name: &'a str,
    // ) -> Option<Context<'a>> {
    //     if let Some(class_context) = self.classes.get(class_name) {
    //         if class_context.methods().iter().any(|m| m.0 == name) {
    //             return Some(Context {
    //                 declarations: self,
    //                 class_name: Some(class_name),
    //                 name,
    //             });
    //         }
    //     }

    //     None
    // }
}

// pub struct Context<'a> {
//     declarations: Rc<Declarations<'a>>,
//     class_name: Option<&'a str>,
//     name: &'a str,
// }

// impl<'a> Context<'a> {
//     pub fn name(&self) -> &'a str {
//         self.name
//     }

//     pub fn class_name(&self) -> Option<&'a str> {
//         self.class_name
//     }

//     fn function_context(&self) -> &Function<'a> {
//         match self.class_name {
//             Some(class) => {
//                 &self
//                     .declarations
//                     .classes()
//                     .get(class)
//                     .expect("this class should exist")
//                     .methods()
//                     .iter()
//                     .find(|m| m.0 == self.name)
//                     .expect("this method should exist")
//                     .1
//             }
//             None => self
//                 .declarations
//                 .functions()
//                 .get(self.name)
//                 .expect("this function should exist"),
//         }
//     }

//     pub fn args(&self) -> &[(&'a str, Type<'a>)] {
//         self.function_context().args()
//     }

//     pub fn ret(&self) -> Option<Type<'a>> {
//         self.function_context().ret()
//     }

//     pub fn get_class(&self, class: &'a str) -> Option<&Class<'a>> {
//         self.declarations.classes().get(class)
//     }

//     pub fn get_function(&self, name: &'a str) -> Option<&Function<'a>> {
//         self.declarations.functions().get(name)
//     }

//     pub fn get_field(&self, name: &'a str) -> Option<(&'a str, Type<'a>)> {
//         self.get_field_of(self.class_name?, name)
//     }

//     pub fn get_field_of(&self, mut class: &'a str, name: &'a str) -> Option<(&'a str, Type<'a>)> {
//         loop {
//             let decl = self.declarations.classes().get(class)?;
//             let res = decl.fields().iter().find(|f| f.0 == name);
//             if let Some((_, t)) = res {
//                 return Some((class, *t));
//             }

//             class = decl.parent()?;
//         }
//     }

//     pub fn get_method(&self, name: &'a str) -> Option<(&'a str, &Function<'a>)> {
//         self.get_method_of(self.class_name?, name)
//     }

//     pub fn get_method_of(
//         &self,
//         mut class: &'a str,
//         name: &'a str,
//     ) -> Option<(&'a str, &Function<'a>)> {
//         loop {
//             let decl = self.declarations.classes().get(class)?;
//             let m = decl.methods().iter().find(|m| m.0 == name);
//             if let Some((_, method)) = m {
//                 return Some((class, method));
//             }

//             class = decl.parent()?;
//         }
//     }

//     pub fn is_subclass(&self, subclass: &str, superclass: &str) -> bool {
//         let mut class = self.declarations.classes().get_key_value(subclass);
//         while let Some((name, decl)) = class {
//             if *name == superclass {
//                 return true;
//             }

//             class = decl
//                 .parent()
//                 .and_then(|parent| self.declarations.classes().get_key_value(parent));
//         }

//         false
//     }
// }

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        ast::{Arg, Def, FnDef, Leaf, Type},
        context::builder::ContextBuilder,
        error::LatteError,
    };

    fn make_ctx<'a>(defs: &[Def<'a>]) -> Result<Context<'a>, LatteError> {
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
