use super::function::Function;
use crate::{
    ast::{ClassDef, FnDef, Type},
    error::LatteError,
};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Method<'a> {
    function: Function<'a>,
    vtable_idx: usize,
    origin_class: &'a str,
}

impl<'a> Method<'a> {
    pub fn as_fun(&self) -> &Function<'a> {
        &self.function
    }

    pub fn origin_class(&self) -> &'a str {
        self.origin_class
    }
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    name: &'a str,
    ty: Type<'a>,
    offset: usize,
}

#[derive(Debug)]
pub struct Class<'a> {
    parent: Option<Rc<Class<'a>>>,
    methods: Vec<Method<'a>>,
    fields: Vec<Field<'a>>,
    name: &'a str,
}

impl<'a> Class<'a> {
    pub fn new(def: &ClassDef<'a>, parent: Option<Rc<Class<'a>>>) -> Result<Self, LatteError> {
        let dup = super::find_duplicate(def.fields.iter().map(|f| f.name));
        if let Some(dup) = dup {
            return Err(LatteError::new_at(
                format!(
                    "field \"{}\" already defined in class \"{}\"",
                    dup.inner, def.ident.inner
                ),
                dup.offset,
            ));
        }

        let dup = super::find_duplicate(def.methods.iter().map(|m| m.ident));
        if let Some(dup) = dup {
            return Err(LatteError::new_at(
                format!(
                    "method \"{}\" already defined in class \"{}\"",
                    dup.inner, dup.inner
                ),
                dup.offset,
            ));
        }

        let mut fields = parent
            .as_ref()
            .map(|p| p.fields.clone())
            .unwrap_or_default();
        for field in &def.fields {
            let mut offset = fields
                .last()
                .map(|f| f.offset + usize::from(f.ty.size()))
                .unwrap_or_default();
            let field_size = usize::from(field.field_type.inner.size());
            if offset % field_size != 0 {
                offset += field_size - (offset % field_size);
            }
            fields.push(Field {
                name: field.name.inner,
                ty: field.field_type.inner,
                offset,
            })
        }

        let mut methods = parent
            .as_ref()
            .map(|p| p.methods.clone())
            .unwrap_or_default();
        for method in &def.methods {
            let function = Function::new(method, Some(def))?;
            let prev = methods
                .iter_mut()
                .find(|m| m.function.name() == function.name());
            match prev {
                Some(prev) => {
                    Self::check_override(&prev.function, method)?;
                    prev.function = function;
                }
                None => {
                    let vtable_idx = methods.last().map(|m| m.vtable_idx + 1).unwrap_or_default();
                    methods.push(Method {
                        function,
                        vtable_idx,
                        origin_class: def.ident.inner,
                    })
                }
            }
        }

        Ok(Self {
            parent,
            fields,
            methods,
            name: def.ident.inner,
        })
    }

    fn check_override(prev: &Function, new: &FnDef) -> Result<(), LatteError> {
        let expected_arguments = prev.args().len() - 1;
        if new.args.len() != expected_arguments {
            return Err(LatteError::new_at(
                format!(
                    "invalid arguments count in method override, expected {} argument(s)",
                    expected_arguments
                ),
                new.offset(),
            ));
        }

        let zipped_args = new.args.iter().zip(prev.args().iter().skip(1));
        for ((new, prev), i) in zipped_args.zip(1..) {
            if new.arg_type.inner != prev.ty {
                return Err(LatteError::new_at(
                    format!(
                        "invalid argument {} type in method override, expected {}",
                        i, prev.ty
                    ),
                    new.arg_type.offset,
                ));
            }
        }

        if prev.ret() != new.ret.map(|l| &l.inner) {
            let msg = match prev.ret() {
                Some(t) => format!("invalid return type in method override, expected {}", t),
                None => "invalid return type in method override, expected void".into(),
            };
            return Err(LatteError::new_at(msg, new.offset()));
        }

        Ok(())
    }

    pub fn methods(&self) -> &[Method<'a>] {
        self.methods.as_slice()
    }

    pub fn name(&self) -> &'a str {
        self.name
    }

    pub fn parent(&self) -> Option<&Class<'a>> {
        self.parent.as_deref()
    }
}
