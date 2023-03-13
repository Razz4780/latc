use super::{function::Function, layout::Layout, Bytes, Context};
use crate::{
    ast::{ClassDef, FnDef, Type},
    context::GetSize,
    error::{self, StaticCheckError},
};
use std::rc::Rc;

/// A class method signature.
#[derive(Debug, Clone)]
pub struct Method<'a> {
    function: Function<'a>,
    vtable_idx: i32,
    origin_class: &'a str,
}

impl<'a> Method<'a> {
    /// Returns the signature of this method.
    pub fn as_fun(&self) -> &Function<'a> {
        &self.function
    }

    /// Returns the index of this method inside the class' vtable.
    pub fn vtable_idx(&self) -> i32 {
        self.vtable_idx
    }

    /// Returns the name of the class containing the definition this method.
    /// This may be some class higher in the inheritance tree.
    pub fn origin_class(&self) -> &'a str {
        self.origin_class
    }
}

/// A class field signature.
#[derive(Debug, Clone)]
pub struct Field<'a> {
    name: &'a str,
    ty: Type<'a>,
    offset: i32,
}

impl<'a> Field<'a> {
    /// Returns the name of this field.
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Returns the type of this field.
    pub fn ty(&self) -> &Type<'a> {
        &self.ty
    }

    /// Returns the offset of this field inside a class instance.
    pub fn offset(&self) -> i32 {
        self.offset
    }
}

/// A class signature.
#[derive(Debug)]
pub struct Class<'a> {
    parent: Option<Rc<Class<'a>>>,
    methods: Vec<Method<'a>>,
    fields: Vec<Field<'a>>,
    name: &'a str,
}

impl<'a> Class<'a> {
    /// Statically checks the given [`ClassDef`] and creates a new instance of this struct.
    /// Does not check statements inside the class methods.
    pub fn new(
        def: &ClassDef<'a>,
        parent: Option<Rc<Class<'a>>>,
    ) -> Result<Self, StaticCheckError> {
        let dup = super::find_duplicate(def.fields.iter().map(|f| f.name));
        if let Some(dup) = dup {
            error::bail!(
                dup.offset,
                "field \"{}\" already defined in class \"{}\"",
                dup.inner,
                def.ident.inner,
            );
        }

        let dup = super::find_duplicate(def.methods.iter().map(|m| m.ident));
        if let Some(dup) = dup {
            error::bail!(
                dup.offset,
                "method \"{}\" already defined in class \"{}\"",
                dup.inner,
                dup.inner,
            );
        }

        let mut fields = parent
            .as_ref()
            .map(|p| p.fields.clone())
            .unwrap_or_default();
        let prev_size = fields
            .last()
            .map(|f| f.offset + i32::from(f.ty.size()))
            .unwrap_or(Context::CLASS_VTABLE_OFFSET + i32::from(Bytes::B8));
        let mut layout = Layout::new(prev_size);
        for field in &def.fields {
            fields.push(Field {
                name: field.name.inner,
                ty: field.field_type.inner,
                offset: layout.add_entry(field.field_type.inner),
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
                    prev.origin_class = def.ident.inner;
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

    fn check_override(prev: &Function, new: &FnDef) -> Result<(), StaticCheckError> {
        let expected_arguments = prev.args().len() - 1;
        if new.args.len() != expected_arguments {
            error::bail!(
                new.offset(),
                "invalid arguments count, expected {} argument(s)",
                expected_arguments,
            );
        }

        let zipped_args = new.args.iter().zip(prev.args().iter().skip(1));
        for ((new, prev), i) in zipped_args.zip(1..) {
            if new.arg_type.inner != prev.ty {
                error::bail!(
                    new.arg_type.offset,
                    "invalid argument {} type in method override, expected {}",
                    i,
                    prev.ty,
                );
            }
        }

        if prev.ret() != new.ret.map(|l| l.inner).as_ref() {
            match prev.ret() {
                Some(t) => error::bail!(
                    new.offset(),
                    "invalid return type in method override, expected {}",
                    t
                ),
                None => error::bail!(
                    new.offset(),
                    "invalid return type in method override, expected void"
                ),
            }
        }

        Ok(())
    }

    /// Returns the [`Method`] with the given name.
    pub fn method(&self, name: &str) -> Option<&Method<'a>> {
        self.methods.iter().find(|m| m.as_fun().name() == name)
    }

    /// Returns the [`Field`] with the given name.
    pub fn field(&self, name: &str) -> Option<&Field<'a>> {
        self.fields.iter().rev().find(|f| f.name() == name)
    }

    /// Returns the name of this class.
    pub fn name(&self) -> &'a str {
        self.name
    }

    /// Returns the parent of this class.
    pub fn parent(&self) -> Option<&Class<'a>> {
        self.parent.as_deref()
    }

    /// Returns the size of this class in bytes.
    pub fn size(&self) -> i32 {
        self.fields
            .last()
            .map(|f| f.offset + i32::from(f.ty().size()))
            .unwrap_or(Bytes::B8.into())
    }

    /// Returns all methods of this class.
    /// This includes methods defined higher in the inheritance tree.
    pub fn methods(&self) -> &[Method<'a>] {
        self.methods.as_slice()
    }
}
