use super::FunctionId;
use crate::context::{Class, Context, Function};

#[derive(Clone, Copy)]
pub struct FnContext<'a, 'b> {
    context: &'b Context<'a>,
    function: &'b Function<'a>,
    class: Option<&'b Class<'a>>,
}

impl<'a, 'b> FnContext<'a, 'b> {
    pub fn class(&self, name: &str) -> Option<&'b Class<'a>> {
        self.context.classes().get(name).map(|c| c.as_ref())
    }

    pub fn function(&self, name: &str) -> Option<&'b Function<'a>> {
        self.context.functions().get(name)
    }

    pub fn current_class(&self) -> Option<&'b Class<'a>> {
        self.class
    }

    pub fn current_function(&self) -> &'b Function<'a> {
        self.function
    }
}

impl<'a> Context<'a> {
    pub fn fn_context<'b>(&'b self, id: &FunctionId<'a>) -> Option<FnContext<'a, 'b>> {
        let (function, class) = match id {
            FunctionId::Global { name } => (self.functions().get(name)?, None),
            FunctionId::Method { name, class } => {
                let class = self.classes().get(class)?;
                let function = class.method(name)?.as_fun();
                (function, Some(class.as_ref()))
            }
        };

        Some(FnContext {
            context: self,
            function,
            class,
        })
    }
}
