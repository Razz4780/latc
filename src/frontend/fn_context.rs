use crate::{ast::Type, context::{Class, Function, Context, Argument}};
use super::FunctionId;

#[derive(Clone, Copy)]
pub struct FnContext<'a, 'b> {
    context: &'b Context<'a>,
    function: &'b Function<'a>,
    class: Option<&'b Class<'a>>,
}

impl<'a, 'b> FnContext<'a, 'b> {
    pub fn args(&self) -> &'b [Argument<'a>] {
        self.function.args()
    }

    pub fn get_class(&self, name: &str) -> Option<&'b Class<'a>> {
        self.context.classes().get(name).map(|c| c.as_ref())
    }

    pub fn ret(&self) -> Option<&'b Type<'a>> {
        self.function.ret()
    }

    pub fn name(&self) -> &'a str {
        self.function.name()
    }

    pub fn class_name(&self) -> Option<&'a str> {
        self.class.map(Class::name)
    }
}

impl<'a> Context<'a> {
    pub fn fn_context<'b>(&'b self, id: &FunctionId<'a>) -> Option<FnContext<'a, 'b>> {
        let (function, class) = match id {
            FunctionId::Global { name } => (self.functions().get(name)?, None),
            FunctionId::Method { name, class } => {
                let class = self.classes().get(class)?;
                let function = class.methods().iter().find(|m| m.as_fun().name() == *name)?.as_fun();
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
