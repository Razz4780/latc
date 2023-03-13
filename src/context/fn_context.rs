use super::FunctionId;
use crate::context::{Class, Context, Function};
use std::ops::Deref;

/// A helper struct for processing a function definition.
#[derive(Clone, Copy)]
pub struct FnContext<'a, 'b> {
    context: &'b Context<'a>,
    function: &'b Function<'a>,
    class: Option<&'b Class<'a>>,
}

impl<'a, 'b> Deref for FnContext<'a, 'b> {
    type Target = Context<'a>;

    fn deref(&self) -> &Self::Target {
        self.context
    }
}

impl<'a, 'b> FnContext<'a, 'b> {
    /// Returns the class scope for the currently processed function.
    pub fn current_class(&self) -> Option<&'b Class<'a>> {
        self.class
    }

    /// Returns the signature for the currently processed function.
    pub fn current_function(&self) -> &'b Function<'a> {
        self.function
    }
}

impl<'a> Context<'a> {
    /// Creates a new [`FnContext`] for checking the function with the given id.
    /// Returns [`None`] if this function does not exist.
    pub fn fn_context<'b>(&'b self, id: &FunctionId<'a>) -> Option<FnContext<'a, 'b>> {
        let (function, class) = match id {
            FunctionId::Global { name } => (self.function(name)?, None),
            FunctionId::Method { name, class } => {
                let class = self.class(class)?;
                let function = class.method(name)?.as_fun();
                (function, Some(class))
            }
        };

        Some(FnContext {
            context: self,
            function,
            class,
        })
    }
}
