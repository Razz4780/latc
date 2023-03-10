use super::{fn_context::FnContext, *};
use crate::{
    ast::{BinOp, Exp, Item, Leaf, SimpleType, Stmt, Type, UnOp},
    context::Function,
    error::{self, StaticCheckError},
};
use std::{
    collections::{HashMap, HashSet},
    mem,
};

pub enum Callable<'a> {
    Static {
        name: &'a str,
    },
    Dynamic {
        method: &'a str,
        object: Expression<'a>,
    },
}

impl<'a> Callable<'a> {
    fn is_dynamic(&self) -> bool {
        matches!(self, Self::Dynamic { .. })
    }
}

#[derive(Clone, Copy)]
struct ExpressionProcessor<'a, 'b> {
    outer: &'b FunctionBuilder<'a, 'b>,
}

impl<'a, 'b> ExpressionProcessor<'a, 'b> {
    fn process_typed(
        self,
        expression: &Exp<'a>,
        expected: Option<Type<'a>>,
    ) -> Result<Expression<'a>, StaticCheckError> {
        let exp = self.process(expression)?;

        match (exp.get_type(), expected) {
            (t1, t2) if t1 == t2 => return Ok(exp),
            (
                Some(Type::Simple(SimpleType::Class(c1))),
                Some(Type::Simple(SimpleType::Class(c2))),
            ) => {
                let mut next = self.outer.ctx.class(c1);
                while let Some(class) = next {
                    if class.name() == c2 {
                        return Ok(exp);
                    }

                    next = class.parent();
                }
            }
            _ => {}
        }

        Err(StaticCheckError::expected_type(
            expected,
            expression.offset(),
        ))
    }

    fn process_callable(
        self,
        expression: &Exp<'a>,
    ) -> Result<(Callable<'a>, &'b Function<'a>), StaticCheckError> {
        match expression {
            Exp::Member { inner, name } => {
                let object = self.process(inner)?;
                let class = object
                    .get_type()
                    .and_then(Type::class_name)
                    .ok_or_else(|| StaticCheckError::expected_class(inner.offset()))?;
                let method = self
                    .outer
                    .ctx
                    .class(class)
                    .unwrap()
                    .method(name.inner)
                    .ok_or_else(|| {
                        error::error!(
                            name.offset,
                            "method \"{}\" not found in class \"{}\"",
                            name.inner,
                            class,
                        )
                    })?;

                Ok((
                    Callable::Dynamic {
                        method: method.as_fun().name(),
                        object,
                    },
                    method.as_fun(),
                ))
            }
            Exp::Var(var) => self
                .outer
                .ctx
                .current_class()
                .and_then(|c| c.method(var.inner))
                .map(|m| {
                    (
                        Callable::Dynamic {
                            method: m.as_fun().name(),
                            object: self.outer.access_local(0),
                        },
                        m.as_fun(),
                    )
                })
                .or_else(|| {
                    let f = self.outer.ctx.function(var.inner)?;
                    Some((Callable::Static { name: f.name() }, f))
                })
                .ok_or_else(|| error::error!(var.offset, "undefined function \"{}\"", var.inner,)),
            _ => error::bail!(expression.offset(), "invalid call operand"),
        }
    }

    fn process_assignable(
        self,
        expression: &Exp<'a>,
    ) -> Result<(Assignable<'a>, Type<'a>), StaticCheckError> {
        match expression {
            Exp::Index { inner, index } => {
                let array = self.process(inner)?;
                let expected_type = array
                    .get_type()
                    .and_then(Type::elem_type)
                    .ok_or_else(|| StaticCheckError::expected_arr(inner.offset()))?;
                let index = self.process_typed(index, Type::INT.into())?;

                Ok((Assignable::slot(array, index), Type::Simple(expected_type)))
            }
            Exp::Member { inner, name } => {
                let object = self.process(inner)?;
                let class = object
                    .get_type()
                    .and_then(Type::class_name)
                    .ok_or_else(|| StaticCheckError::expected_class(inner.offset()))?;
                self.outer
                    .ctx
                    .class(class)
                    .unwrap()
                    .field(name.inner)
                    .map(|f| (Assignable::field(f.name(), object), *f.ty()))
                    .ok_or_else(|| {
                        error::error!(
                            name.offset,
                            "field \"{}\" not found in class \"{}\"",
                            name.inner,
                            class,
                        )
                    })
            }
            Exp::Var(var) => self
                .get_var(var.inner)
                .map(|l| (Assignable::Local { var_idx: l }, self.outer.var_types[l]))
                .or_else(|| {
                    self.outer.ctx.current_class()?.field(var.inner).map(|f| {
                        (
                            Assignable::field(f.name(), self.outer.access_local(0)),
                            *f.ty(),
                        )
                    })
                })
                .ok_or_else(|| error::error!(var.offset, "undefined variable \"{}\"", var.inner,)),
            _ => error::bail!(expression.offset(), "invalid assignment operand",),
        }
    }

    fn process_binary(
        self,
        lhs: &Exp<'a>,
        op: BinOp,
        rhs: &Exp<'a>,
    ) -> Result<Expression<'a>, StaticCheckError> {
        let left = self.process(lhs)?;
        let left_type = left
            .get_type()
            .ok_or_else(|| error::error!(lhs.offset(), "expected a value"))?;

        let expression = match (left_type, op) {
            (Type::BOOL, BinOp::And | BinOp::Or) => {
                let right = self.process_typed(rhs, Type::BOOL.into())?;
                match (left, op, right) {
                    (Expression::LitBool(false), BinOp::And, _) => Expression::LitBool(false),
                    (Expression::LitBool(true), BinOp::And, right) => right,
                    (Expression::LitBool(false), BinOp::Or, right) => right,
                    (Expression::LitBool(true), BinOp::Or, _) => Expression::LitBool(true),
                    (left, op, right) => Expression::Binary {
                        lhs: left.into(),
                        op,
                        rhs: right.into(),
                        value_type: SimpleType::Bool,
                    },
                }
            }
            (_, BinOp::And | BinOp::Or) => {
                return Err(StaticCheckError::expected_type(
                    Some(Type::BOOL),
                    lhs.offset(),
                ))
            }
            (_, BinOp::EQ | BinOp::NEQ) => {
                let right = self.process_typed(rhs, left_type.into())?;
                match (left, right) {
                    (Expression::Null(..), Expression::Null(..)) => Expression::LitBool(true),
                    (Expression::LitBool(b1), Expression::LitBool(b2)) => {
                        Expression::LitBool((b1 == b2) == (op == BinOp::EQ))
                    }
                    (Expression::LitInt(i1), Expression::LitInt(i2)) => {
                        Expression::LitBool((i1 == i2) == (op == BinOp::EQ))
                    }
                    (Expression::LitStr(s1), Expression::LitStr(s2)) => {
                        Expression::LitBool((s1 == s2) == (op == BinOp::EQ))
                    }
                    (left, right) if left_type == Type::STR => {
                        let call = Expression::Call {
                            addr: CallAddr::CmpStrings,
                            args: vec![left, right],
                            ret: Type::INT.into(),
                        };
                        Expression::Binary {
                            lhs: call.into(),
                            op,
                            rhs: Expression::LitInt(0).into(),
                            value_type: SimpleType::Bool,
                        }
                    }
                    (left, right) => Expression::Binary {
                        lhs: left.into(),
                        op,
                        rhs: right.into(),
                        value_type: SimpleType::Bool,
                    },
                }
            }
            (Type::STR, BinOp::Add) => {
                let right = self.process_typed(rhs, Type::STR.into())?;
                Expression::Call {
                    addr: CallAddr::AddStrings,
                    args: vec![left, right],
                    ret: Type::STR.into(),
                }
            }
            (Type::INT, _) => {
                let right = self.process_typed(rhs, Type::INT.into())?;
                match (left, right) {
                    (Expression::LitInt(i1), Expression::LitInt(0))
                        if matches!(op, BinOp::Div | BinOp::Mod) =>
                    {
                        Expression::Binary {
                            lhs: Expression::LitInt(i1).into(),
                            op,
                            rhs: Expression::LitInt(0).into(),
                            value_type: SimpleType::Int,
                        }
                    }
                    (Expression::LitInt(i1), Expression::LitInt(i2)) => match op {
                        BinOp::Add => Expression::LitInt(i1.wrapping_add(i2)),
                        BinOp::Sub => Expression::LitInt(i1.wrapping_sub(i2)),
                        BinOp::Mul => Expression::LitInt(i1.wrapping_mul(i2)),
                        BinOp::Div => Expression::LitInt(i1 / i2),
                        BinOp::Mod => Expression::LitInt(i1 % i2),
                        BinOp::LE => Expression::LitBool(i1 <= i2),
                        BinOp::LTH => Expression::LitBool(i1 < i2),
                        BinOp::GE => Expression::LitBool(i1 >= i2),
                        BinOp::GTH => Expression::LitBool(i1 > i2),
                        _ => unreachable!("this option should already be checked"),
                    },
                    (left, right) => Expression::Binary {
                        lhs: left.into(),
                        op,
                        rhs: right.into(),
                        value_type: match op {
                            BinOp::Add | BinOp::Div | BinOp::Mod | BinOp::Sub | BinOp::Mul => {
                                SimpleType::Int
                            }
                            BinOp::LE | BinOp::LTH | BinOp::GE | BinOp::GTH => SimpleType::Bool,
                            _ => unreachable!("this option should already be checked"),
                        },
                    },
                }
            }
            (_, BinOp::Add) => error::bail!(
                lhs.offset(),
                "invalid expression type, expected int or string",
            ),
            (_, _) => {
                return Err(StaticCheckError::expected_type(
                    Type::INT.into(),
                    lhs.offset(),
                ))
            }
        };

        Ok(expression)
    }

    fn process_application(
        self,
        function: &Exp<'a>,
        arguments: &[Exp<'a>],
    ) -> Result<Expression<'a>, StaticCheckError> {
        let (callable, decl) = self.process_callable(function)?;
        let expected = if callable.is_dynamic() {
            &decl.args()[1..]
        } else {
            decl.args()
        };

        if arguments.len() != expected.len() {
            error::bail!(
                function.offset(),
                "invalid argument count in a function call, expected {} argument(s)",
                expected.len(),
            );
        }

        let mut args = arguments
            .iter()
            .zip(expected)
            .map(|(arg, expected)| self.process_typed(arg, expected.ty.into()))
            .collect::<Result<Vec<_>, StaticCheckError>>()?;

        match callable {
            Callable::Dynamic { method, object } => {
                args.insert(0, object);

                Ok(Expression::Call {
                    addr: CallAddr::Dynamic { method },
                    args,
                    ret: decl.ret().copied(),
                })
            }
            Callable::Static { name } => Ok(Expression::Call {
                addr: CallAddr::Static { name },
                args,
                ret: decl.ret().copied(),
            }),
        }
    }

    fn process(self, expression: &Exp<'a>) -> Result<Expression<'a>, StaticCheckError> {
        let result = match expression {
            Exp::App {
                function,
                arguments,
            } => self.process_application(function, arguments)?,
            Exp::Bin { lhs, op, rhs } => self.process_binary(lhs, op.inner, rhs)?,
            Exp::Bool(val) => Expression::LitBool(val.inner),
            Exp::Index { inner, index } => {
                let arr = self.process(inner)?;
                let value_type = arr
                    .get_type()
                    .and_then(Type::elem_type)
                    .ok_or_else(|| StaticCheckError::expected_arr(inner.offset()))?;

                let idx = self.process_typed(index, Type::INT.into())?;

                Expression::Slot {
                    array: arr.into(),
                    index: idx.into(),
                    value_type,
                }
            }
            Exp::Int(val) => Expression::LitInt(val.inner),
            Exp::Member { inner, name } => {
                let object = self.process(inner)?;

                if name.inner == "length" && object.get_type().and_then(Type::elem_type).is_some() {
                    Expression::Length {
                        array: object.into(),
                    }
                } else {
                    let class = object
                        .get_type()
                        .and_then(Type::class_name)
                        .ok_or_else(|| {
                            error::error!(
                                inner.offset(),
                                "invalid expression type, expected a class or an array",
                            )
                        })?;
                    let field = self
                        .outer
                        .ctx
                        .class(class)
                        .and_then(|c| c.field(name.inner))
                        .ok_or_else(|| {
                            error::error!(
                                name.offset,
                                "field \"{}\" not found in class \"{}\"",
                                name.inner,
                                class
                            )
                        })?;

                    Expression::Field {
                        field: field.name(),
                        object: object.into(),
                        value_type: *field.ty(),
                    }
                }
            }
            Exp::New { inner, .. } => {
                let value_type = Leaf {
                    inner: Type::Simple(inner.inner),
                    offset: inner.offset,
                };
                let class = value_type
                    .inner
                    .class_name()
                    .ok_or_else(|| StaticCheckError::expected_class(inner.offset))?;
                self.outer.check_type(&value_type)?;

                Expression::Call {
                    addr: CallAddr::NewObject { class },
                    args: vec![],
                    ret: value_type.inner.into(),
                }
            }
            Exp::NewArr { inner, size, .. } => {
                let arr_type = Leaf {
                    inner: Type::Arr(inner.inner),
                    offset: inner.offset,
                };
                self.outer.check_type(&arr_type)?;
                let size_expr = self.process_typed(size, Type::INT.into())?;

                Expression::Call {
                    addr: CallAddr::NewArray { inner: inner.inner },
                    args: vec![size_expr],
                    ret: arr_type.inner.into(),
                }
            }
            Exp::Null { cast, .. } => {
                if cast.inner.class_name().is_none() && cast.inner.elem_type().is_none() {
                    error::bail!(cast.offset, "invalid type, expected a class or an array",);
                }
                self.outer.check_type(cast)?;

                Expression::Null(cast.inner)
            }
            Exp::Str(val) => Expression::LitStr(val.inner),
            Exp::Un { op, inner } => {
                let expected = match op.inner {
                    UnOp::Neg => Type::INT,
                    UnOp::Not => Type::BOOL,
                };

                let exp = self.process_typed(inner, expected.into())?;

                match exp {
                    Expression::LitBool(val) => Expression::LitBool(!val),
                    Expression::LitInt(val) => Expression::LitInt(-val),
                    exp => Expression::Unary {
                        op: op.inner,
                        exp: exp.into(),
                    },
                }
            }
            Exp::Var(var) => self
                .get_var(var.inner)
                .map(|l| self.outer.access_local(l))
                .or_else(|| {
                    self.outer
                        .ctx
                        .current_class()
                        .and_then(|c| c.field(var.inner))
                        .map(|f| Expression::Field {
                            field: f.name(),
                            object: self.outer.access_local(0).into(),
                            value_type: *f.ty(),
                        })
                })
                .ok_or_else(|| error::error!(var.offset, "undefined variable \"{}\"", var.inner))?,
        };

        Ok(result)
    }

    fn get_var(&self, name: &'a str) -> Option<usize> {
        for scope in self.outer.scope.iter().rev() {
            let loc = scope.vars.get(name).copied();
            if loc.is_some() {
                return loc;
            }
        }

        None
    }
}

#[derive(Default)]
struct Scope<'a> {
    locked_vars: HashSet<&'a str>,
    vars: HashMap<&'a str, usize>,
}

pub struct FunctionBuilder<'a, 'b> {
    ctx: FnContext<'a, 'b>,
    stmts: Vec<Statement<'a>>,
    scope: Vec<Scope<'a>>,
    var_types: Vec<Type<'a>>,
}

impl<'a, 'b> FunctionBuilder<'a, 'b> {
    pub fn new(ctx: FnContext<'a, 'b>) -> Self {
        let args = ctx.current_function().args();

        let mut scope = Scope::default();
        scope.vars.reserve(args.len());
        let mut var_types = Vec::with_capacity(args.len());

        for arg in ctx.current_function().args() {
            scope.vars.insert(arg.name, var_types.len());
            var_types.push(arg.ty);
        }

        Self {
            ctx,
            stmts: Default::default(),
            scope: vec![scope],
            var_types,
        }
    }

    fn expr(&self) -> ExpressionProcessor<'a, '_> {
        ExpressionProcessor { outer: self }
    }

    fn check_type(&self, ty: &Leaf<Type<'a>>) -> Result<(), StaticCheckError> {
        if let Some(class) = ty.inner.referenced_class() {
            if self.ctx.class(class).is_none() {
                return Err(StaticCheckError::undefined_type(ty.inner, ty.offset));
            }
        }

        Ok(())
    }

    fn access_local(&self, register: usize) -> Expression<'a> {
        Expression::Local {
            location: register,
            value_type: self.var_types[register],
        }
    }

    fn new_var(&mut self, name: &'a str, of_type: Type<'a>) -> Option<usize> {
        let scope = self
            .scope
            .last_mut()
            .expect("programmer error, scope stack should never be empty");
        let locked = !scope.locked_vars.insert(name);
        if locked {
            return None;
        }

        let location = self.var_types.len();
        scope.vars.insert(name, location);
        self.var_types.push(of_type);

        Some(location)
    }

    fn new_anon_var(&mut self, of_type: Type<'a>) -> usize {
        let location = self.var_types.len();
        self.var_types.push(of_type);

        location
    }

    fn process_substatement(
        &mut self,
        statement: Stmt<'a>,
        buffer: &mut Vec<Statement<'a>>,
    ) -> Result<(), StaticCheckError> {
        mem::swap(&mut self.stmts, buffer);
        self.scope.push(Default::default());

        self.add_statement(statement)?;

        self.scope.pop();
        mem::swap(&mut self.stmts, buffer);

        Ok(())
    }

    fn process_mutation(&mut self, loc: &Exp<'a>, change: i32) -> Result<(), StaticCheckError> {
        let (destination, expected_type) = self.expr().process_assignable(loc)?;
        if expected_type != Type::INT {
            return Err(StaticCheckError::expected_type(
                Type::INT.into(),
                loc.offset(),
            ));
        }

        match destination {
            Assignable::Field { field, object } => {
                let ty = object.get_type().unwrap();
                let register = self.new_anon_var(ty);
                self.stmts.push(Statement::Assign {
                    destination: Assignable::local(register),
                    expression: *object,
                });
                let object = self.access_local(register);
                self.stmts.push(Statement::Assign {
                    destination: Assignable::field(field, object.clone()),
                    expression: Expression::Binary {
                        lhs: Expression::Field {
                            field,
                            object: object.into(),
                            value_type: Type::INT,
                        }
                        .into(),
                        op: BinOp::Add,
                        rhs: Expression::LitInt(change).into(),
                        value_type: SimpleType::Int,
                    },
                })
            }
            Assignable::Local { var_idx } => self.stmts.push(Statement::Assign {
                destination: Assignable::local(var_idx),
                expression: Expression::Binary {
                    lhs: self.access_local(var_idx).into(),
                    op: BinOp::Add,
                    rhs: Expression::LitInt(change).into(),
                    value_type: SimpleType::Int,
                },
            }),
            Assignable::Slot { array, index } => {
                let int_array = Type::Arr(SimpleType::Int);
                let array_register = self.new_anon_var(int_array);
                self.stmts.push(Statement::Assign {
                    destination: Assignable::local(array_register),
                    expression: *array,
                });
                let index_register = self.new_anon_var(Type::INT);
                self.stmts.push(Statement::Assign {
                    destination: Assignable::local(index_register),
                    expression: *index,
                });
                self.stmts.push(Statement::Assign {
                    destination: Assignable::slot(
                        self.access_local(array_register),
                        self.access_local(index_register),
                    ),
                    expression: Expression::Binary {
                        lhs: Expression::Slot {
                            array: self.access_local(array_register).into(),
                            index: self.access_local(index_register).into(),
                            value_type: SimpleType::Int,
                        }
                        .into(),
                        op: BinOp::Add,
                        rhs: Expression::LitInt(change).into(),
                        value_type: SimpleType::Int,
                    },
                })
            }
        }

        Ok(())
    }

    fn process_for(
        &mut self,
        elem_type: Leaf<SimpleType>,
        elem_ident: Leaf<&'a str>,
        array: Exp<'a>,
        inner: Stmt<'a>,
    ) -> Result<(), StaticCheckError> {
        let array_exp = self.expr().process(&array)?;
        let expected_type = array_exp
            .get_type()
            .and_then(|t| t.elem_type())
            .ok_or_else(|| StaticCheckError::expected_arr(array.offset()))?;
        if elem_type.inner != expected_type {
            return Err(StaticCheckError::expected_type(
                Type::Simple(expected_type).into(),
                elem_type.offset,
            ));
        }

        let array_type = Type::Arr(expected_type);

        let array_register = self.new_anon_var(array_type);
        self.stmts.push(Statement::Assign {
            destination: Assignable::local(array_register),
            expression: array_exp,
        });

        let index_register = self.new_anon_var(Type::INT);
        self.stmts.push(Statement::Assign {
            destination: Assignable::local(index_register),
            expression: Expression::LitInt(0),
        });

        let cond = Expression::Binary {
            lhs: self.access_local(index_register).into(),
            op: BinOp::NEQ,
            rhs: Expression::Length {
                array: self.access_local(array_register).into(),
            }
            .into(),
            value_type: SimpleType::Bool,
        };

        self.scope.push(Default::default());

        let elem_register = self
            .new_var(elem_ident.inner, Type::Simple(expected_type))
            .expect("new scope has just been opened, this name should not be locked");

        let mut inner_buf = vec![Statement::Assign {
            destination: Assignable::local(elem_register),
            expression: Expression::Slot {
                array: self.access_local(array_register).into(),
                index: self.access_local(index_register).into(),
                value_type: expected_type,
            },
        }];
        self.process_substatement(inner, &mut inner_buf)?;
        inner_buf.push(Statement::Assign {
            destination: Assignable::local(index_register),
            expression: Expression::Binary {
                lhs: self.access_local(index_register).into(),
                op: BinOp::Add,
                rhs: Expression::LitInt(1).into(),
                value_type: SimpleType::Int,
            },
        });

        self.scope.pop();

        self.stmts.push(Statement::While {
            cond,
            inner: inner_buf,
        });

        Ok(())
    }

    pub fn add_statement(&mut self, statement: Stmt<'a>) -> Result<(), StaticCheckError> {
        match statement {
            Stmt::Empty => {}
            Stmt::Block { inner, .. } => {
                self.scope.push(Default::default());

                for stmt in inner {
                    self.add_statement(stmt)?;
                }

                self.scope.pop();
            }
            Stmt::Ass { loc, exp } => {
                let (destination, expected_type) = self.expr().process_assignable(&loc)?;
                let expression = self.expr().process_typed(&exp, expected_type.into())?;
                self.stmts.push(Statement::Assign {
                    destination,
                    expression,
                });
            }
            Stmt::Cond { cond, inner, .. } => {
                let expression = self.expr().process_typed(&cond, Type::BOOL.into())?;
                let mut inner_if = Default::default();
                self.process_substatement(*inner, &mut inner_if)?;
                self.stmts.push(Statement::Cond {
                    cond: expression,
                    inner_if,
                    inner_else: Default::default(),
                });
            }
            Stmt::CondElse {
                cond,
                inner_if,
                inner_else,
                ..
            } => {
                let expression = self.expr().process_typed(&cond, Type::BOOL.into())?;

                let mut inner_if_buf = Default::default();
                self.process_substatement(*inner_if, &mut inner_if_buf)?;
                let mut inner_else_buf = Default::default();
                self.process_substatement(*inner_else, &mut inner_else_buf)?;

                self.stmts.push(Statement::Cond {
                    cond: expression,
                    inner_if: inner_if_buf,
                    inner_else: inner_else_buf,
                });
            }
            Stmt::Decl { item_type, items } => {
                self.check_type(&item_type)?;

                for Item { var, exp } in items {
                    let expression = match exp {
                        Some(exp) => self.expr().process_typed(&exp, item_type.inner.into())?,
                        None => Expression::zero(item_type.inner),
                    };

                    let register = self.new_var(var.inner, item_type.inner).ok_or_else(|| {
                        error::error!(var.offset, "variable \"{}\" already declared", var.inner,)
                    })?;

                    self.stmts.push(Statement::Assign {
                        destination: Assignable::local(register),
                        expression,
                    });
                }
            }
            Stmt::Decr { loc } => self.process_mutation(&loc, -1)?,
            Stmt::For {
                elem_type,
                elem_ident,
                arr,
                inner,
                ..
            } => self.process_for(elem_type, elem_ident, arr, *inner)?,
            Stmt::Ret { offset, exp } => match exp {
                Some(exp) => {
                    let ret = self.ctx.current_function().ret().copied();
                    let expression = self.expr().process_typed(&exp, ret)?;
                    self.stmts.push(Statement::Return(expression));
                }
                None => {
                    if self.ctx.current_function().ret().is_some() {
                        error::bail!(offset, "non-void function must return a value",);
                    }

                    self.stmts.push(Statement::Return(Expression::Void));
                }
            },
            Stmt::While { cond, inner, .. } => {
                let expression = self.expr().process_typed(&cond, Type::BOOL.into())?;
                let mut inner_buf = Default::default();
                self.process_substatement(*inner, &mut inner_buf)?;

                self.stmts.push(Statement::While {
                    cond: expression,
                    inner: inner_buf,
                });
            }
            Stmt::Incr { loc } => self.process_mutation(&loc, 1)?,
            Stmt::Exp(exp) => {
                let expression = self.expr().process(&exp)?;
                self.stmts.push(Statement::Expression(expression));
            }
        }

        Ok(())
    }

    pub fn finish(self) -> Result<Vec<Statement<'a>>, StaticCheckError> {
        let (mut statements, stops) = simplify_flow(self.stmts);

        let statements = match (
            self.ctx.current_function().ret(),
            stops,
            self.ctx.current_class(),
        ) {
            (Some(t), false, None) => error::bail!(
                None,
                "function \"{}\" must return a value of type {}",
                self.ctx.current_function().name(),
                t
            ),
            (Some(t), false, Some(class)) => error::bail!(
                None,
                "method \"{}\" defined in class \"{}\" must return a value of type {}",
                self.ctx.current_function().name(),
                class.name(),
                t
            ),
            (None, false, _) => {
                statements.push(Statement::Return(Expression::Void));
                statements
            }
            _ => statements,
        };

        Ok(statements)
    }
}

fn simplify_flow(statements: Vec<Statement<'_>>) -> (Vec<Statement<'_>>, bool) {
    let mut statements_new = Vec::with_capacity(statements.len());

    for statement in statements {
        match statement {
            Statement::Return(exp) => {
                statements_new.push(Statement::Return(exp));
                return (statements_new, true);
            }
            Statement::Cond {
                cond: Expression::LitBool(true),
                inner_if,
                ..
            } => {
                let (simpler, stops) = simplify_flow(inner_if);
                statements_new.extend(simpler);
                if stops {
                    return (statements_new, true);
                }
            }
            Statement::Cond {
                cond: Expression::LitBool(false),
                inner_else,
                ..
            } => {
                let (simpler, stops) = simplify_flow(inner_else);
                statements_new.extend(simpler);
                if stops {
                    return (statements_new, true);
                }
            }
            Statement::Cond {
                cond,
                inner_if,
                inner_else,
            } => {
                let (inner_if, if_stops) = simplify_flow(inner_if);
                let (inner_else, else_stops) = simplify_flow(inner_else);

                statements_new.push(Statement::Cond {
                    cond,
                    inner_if,
                    inner_else,
                });

                if if_stops && else_stops {
                    return (statements_new, true);
                }
            }
            Statement::While {
                cond: Expression::LitBool(false),
                ..
            } => {}
            Statement::While {
                cond: Expression::LitBool(true),
                inner,
            } => {
                statements_new.push(Statement::While {
                    cond: Expression::LitBool(true),
                    inner,
                });
                return (statements_new, true);
            }
            Statement::Expression(e) => {
                let stops = e.exits();
                statements_new.push(Statement::Expression(e));
                if stops {
                    return (statements_new, true);
                }
            }
            s => statements_new.push(s),
        }
    }

    (statements_new, false)
}
