mod opts;

use super::{
    context::{Class, Context, Function, Symbol},
    hir::{BinOp, BlockId, Hir, PhiOption, RelCond, VReg, VRegId, Value},
    size::{Bytes, GetSize},
};
use crate::{
    ast::{self, UnOp},
    frontend::{Assignable, CallAddr, Expression, Statement},
};
use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{self, Debug, Formatter},
    mem,
    ops::RangeFrom,
};

#[derive(Default, Clone, Debug)]
pub struct Edges {
    children: Vec<BlockId>,
    parents: Vec<BlockId>,
}

impl Edges {
    pub fn children(&self) -> &[BlockId] {
        self.children.as_slice()
    }

    pub fn parents(&self) -> &[BlockId] {
        self.parents.as_slice()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HirLoc {
    pub block: BlockId,
    pub hir_idx: usize,
}

impl Debug for HirLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.block, self.hir_idx)
    }
}

pub struct HirFunction<'a> {
    blocks: Vec<Vec<Hir<'a>>>,
    cfg: Vec<Edges>,
}

impl<'a> HirFunction<'a> {
    pub fn block(&self, id: BlockId) -> &[Hir<'a>] {
        self.blocks[id.0].as_slice()
    }

    pub fn blocks(&self) -> &[Vec<Hir<'a>>] {
        self.blocks.as_slice()
    }

    pub fn cfg(&self) -> &[Edges] {
        self.cfg.as_slice()
    }

    pub fn parents_of(&self, id: BlockId) -> &[BlockId] {
        self.cfg[id.0].parents()
    }

    pub fn children_of(&self, id: BlockId) -> &[BlockId] {
        self.cfg[id.0].children()
    }
}

struct RegGenerator(RangeFrom<usize>);

impl Default for RegGenerator {
    fn default() -> Self {
        Self(0..)
    }
}

impl RegGenerator {
    pub fn gen(&mut self, size: Bytes) -> VReg {
        VReg {
            id: VRegId(self.0.next().expect("no more unique identifiers")),
            size,
        }
    }
}

#[derive(Default, Debug)]
pub struct BlockBuilder<'a> {
    prev_blocks: Vec<BlockId>,
    defined_vars: HashMap<usize, Value<'a>>,
    hirs: Vec<Hir<'a>>,
    phis: HashMap<usize, (VReg, [PhiOption<'a>; 2])>,
    incomplete_phis: HashMap<usize, VReg>,
    header_start: bool,
    join_hint: Option<BlockId>,
}

impl<'a> BlockBuilder<'a> {
    fn emit(&mut self, hir: Hir<'a>) {
        self.hirs.push(hir)
    }

    fn get_val(&self, var: usize) -> Option<Value<'a>> {
        self.defined_vars.get(&var).copied()
    }

    fn define(&mut self, var: usize, val: Value<'a>) {
        self.defined_vars.insert(var, val);
    }

    fn try_define(&mut self, var: usize, val: Value<'a>) {
        if let Entry::Vacant(e) = self.defined_vars.entry(var) {
            e.insert(val);
        }
    }

    fn set_var_phi(&mut self, var: usize, reg: VReg, options: [PhiOption<'a>; 2]) {
        self.incomplete_phis.remove(&var);
        self.phis.insert(var, (reg, options));
    }

    fn prepare_var_phi(&mut self, var: usize, reg: VReg) {
        self.incomplete_phis.insert(var, reg);
    }

    fn finalize_phis(mut self) -> Vec<Hir<'a>> {
        let mut hirs = Vec::with_capacity(self.phis.len() + self.hirs.len());

        hirs.extend(
            self.phis
                .into_values()
                .map(|(dst, options)| Hir::Phi { dst, options }),
        );
        hirs.extend(mem::take(&mut self.hirs));

        hirs
    }
}

pub struct HirGenerator<'a, 'b> {
    context: &'b Context<'a>,
    reg_gen: RegGenerator,
    blocks: Vec<BlockBuilder<'a>>,
    active_block: BlockId,
}

impl<'a, 'b> HirGenerator<'a, 'b> {
    pub fn new(fun: &Function<'a>, context: &'b Context<'a>) -> Self {
        let mut reg_gen = RegGenerator::default();

        let mut block = BlockBuilder::default();
        for (var, size) in fun.args().enumerate() {
            let reg = reg_gen.gen(size);
            block.define(var, reg.into());
        }

        Self {
            context,
            reg_gen,
            blocks: vec![block],
            active_block: BlockId(0),
        }
    }

    fn get_val(&mut self, var: usize, size: Bytes, block_id: BlockId) -> Value<'a> {
        let block = self.block(block_id);
        if let Some(reg) = block.get_val(var) {
            return reg;
        }

        if block.header_start {
            let reg = self.reg_gen.gen(size);
            let block = self.block(block_id);
            block.prepare_var_phi(var, reg);
            block.define(var, reg.into());
            return reg.into();
        }

        match *block.prev_blocks.as_slice() {
            [prev] => {
                let reg = self.get_val(var, size, prev);
                self.block(block_id).define(var, reg);
                reg
            }
            [prev_1, prev_2] => {
                let val_1 = self.get_val(var, size, prev_1);
                let val_2 = self.get_val(var, size, prev_2);
                if val_1 == val_2 {
                    self.block(block_id).define(var, val_1);
                    val_1
                } else {
                    let reg = self.reg_gen.gen(size);
                    let block = self.block(block_id);
                    block.set_var_phi(
                        var,
                        reg,
                        [
                            PhiOption {
                                val: val_1,
                                from: prev_1,
                            },
                            PhiOption {
                                val: val_2,
                                from: prev_2,
                            },
                        ],
                    );
                    block.define(var, reg.into());
                    reg.into()
                }
            }
            _ => unreachable!(),
        }
    }

    fn active_block(&mut self) -> &mut BlockBuilder<'a> {
        &mut self.blocks[self.active_block.0]
    }

    fn block(&mut self, id: BlockId) -> &mut BlockBuilder<'a> {
        &mut self.blocks[id.0]
    }

    fn new_simple_block(&mut self, parent: BlockId) -> BlockId {
        let id = self.blocks.len();

        self.blocks.push(BlockBuilder {
            prev_blocks: vec![parent],
            ..Default::default()
        });

        BlockId(id)
    }

    fn new_join_block(&mut self, left_parent: BlockId, right_parent: BlockId) -> BlockId {
        let id = self.blocks.len();

        self.blocks.push(BlockBuilder {
            prev_blocks: vec![left_parent, right_parent],
            ..Default::default()
        });

        BlockId(id)
    }

    fn new_header_block(&mut self) -> BlockId {
        let id = self.blocks.len();

        self.blocks.push(BlockBuilder {
            header_start: true,
            ..Default::default()
        });

        BlockId(id)
    }

    fn process_bool_expr(
        &mut self,
        lhs: &Expression<'a>,
        is_and: bool,
        rhs: &Expression<'a>,
    ) -> Value<'a> {
        let lhs = self.process_expr(lhs).unwrap();
        let block_lhs_last = self.active_block;
        let block_rhs_first = self.new_simple_block(block_lhs_last);

        self.active_block = block_rhs_first;
        let rhs = self.process_expr(rhs).unwrap();
        let block_rhs_last = self.active_block;

        let block_cont = self.new_join_block(block_lhs_last, block_rhs_last);

        let from_lhs_val = if is_and {
            self.block(block_lhs_last).emit(Hir::CondJmp {
                val: lhs,
                dst_if: block_rhs_first,
                dst_else: block_cont,
            });
            Value::FALSE
        } else {
            self.block(block_lhs_last).emit(Hir::CondJmp {
                val: lhs,
                dst_if: block_cont,
                dst_else: block_rhs_first,
            });
            Value::TRUE
        };
        self.block(block_lhs_last).join_hint.replace(block_cont);
        self.block(block_rhs_last)
            .emit(Hir::Jmp { dst: block_cont });

        self.active_block = block_cont;
        let result = self.reg_gen.gen(Bytes::B1);
        self.active_block().emit(Hir::Phi {
            dst: result,
            options: [
                PhiOption {
                    val: from_lhs_val,
                    from: block_lhs_last,
                },
                PhiOption {
                    val: rhs,
                    from: block_rhs_last,
                },
            ],
        });

        result.into()
    }

    fn process_binary_expr(
        &mut self,
        lhs: &Expression<'a>,
        op: ast::BinOp,
        rhs: &Expression<'a>,
    ) -> Value<'a> {
        if let Ok(op) = BinOp::try_from(op) {
            let lhs = self.process_expr(lhs).unwrap();
            let rhs = self.process_expr(rhs).unwrap();
            let result = self.reg_gen.gen(lhs.size());
            self.active_block().emit(Hir::Bin {
                lhs,
                op,
                rhs,
                dst: result,
            });
            result.into()
        } else if let Ok(cond) = RelCond::try_from(op) {
            let lhs = self.process_expr(lhs).unwrap();
            let rhs = self.process_expr(rhs).unwrap();
            let result = self.reg_gen.gen(Bytes::B1);
            self.active_block().emit(Hir::Cmp {
                dst: result,
                cond,
                lhs,
                rhs,
            });
            result.into()
        } else {
            self.process_bool_expr(lhs, op == ast::BinOp::And, rhs)
        }
    }

    fn process_slot_expr(
        &mut self,
        array: &Expression<'a>,
        index: &Expression<'a>,
        elem_size: Bytes,
    ) -> Value<'a> {
        let array = self.process_expr(array).unwrap();
        let index = self.process_expr(index).unwrap();
        let elems = self.reg_gen.gen(Bytes::B8);
        self.active_block().emit(Hir::Load {
            base: array,
            index: None,
            displacement: Context::ARRAY_ELEMS_OFFSET,
            dst: elems,
        });

        let imm_offset = match &index {
            Value::ImmB1(i) => Some(i32::from(*i)),
            Value::ImmB4(i) => Some(*i),
            Value::ImmB8(i) => i32::try_from(*i).ok(),
            _ => None,
        }
        .and_then(|i| i.checked_mul(elem_size.into()));

        let elem = self.reg_gen.gen(elem_size);

        match imm_offset {
            Some(displacement) => {
                self.active_block().emit(Hir::Load {
                    base: elems.into(),
                    index: None,
                    displacement,
                    dst: elem,
                });
            }
            None => {
                self.active_block().emit(Hir::Load {
                    base: elems.into(),
                    index: Some((index, elem_size)),
                    displacement: 0,
                    dst: elem,
                });
            }
        }

        elem.into()
    }

    fn process_field_expr(
        &mut self,
        class: &str,
        field: &str,
        object: &Expression<'a>,
    ) -> Value<'a> {
        let field = self.context.class(class).field(field);

        let object = self.process_expr(object).unwrap();
        let result = self.reg_gen.gen(field.of_type().size());

        self.active_block().emit(Hir::Load {
            base: object,
            index: None,
            displacement: field.offset(),
            dst: result,
        });

        result.into()
    }

    fn process_call_expr(
        &mut self,
        addr: &CallAddr<'a>,
        mut args: &[Expression<'a>],
    ) -> Option<Value<'a>> {
        let mut processed_args = Vec::with_capacity(args.len());

        let (fun, ret): (Value<'a>, Option<Bytes>) = match addr {
            CallAddr::AddStrings => (Symbol::AddStrings.into(), Bytes::B8.into()),
            CallAddr::CmpStrings => (Symbol::CmpStrings.into(), Bytes::B8.into()),
            CallAddr::NewArray { inner } => {
                let size = inner.size();
                processed_args.push(Value::ImmB4(size.into()));
                (Symbol::NewArray.into(), Bytes::B8.into())
            }
            CallAddr::NewObject { class } => {
                let size = self.context.class(class).size();
                processed_args.push(Value::ImmB4(size));
                processed_args.push(Value::Symbol(Symbol::VTable(class)));
                (Symbol::NewObject.into(), Bytes::B8.into())
            }
            CallAddr::Static { name } => {
                let function = self.context.function(name);
                (
                    Symbol::Function(name).into(),
                    function.ret().as_ref().map(GetSize::size),
                )
            }
            CallAddr::Dynamic { class, method } => {
                let method = self.context.class(class).method(method);

                let object = self.process_expr(&args[0]).unwrap();
                processed_args.push(object);
                args = &args[1..];

                let vtable = self.reg_gen.gen(Bytes::B8);
                self.active_block().emit(Hir::Load {
                    base: object,
                    index: None,
                    displacement: Class::vtable_offset(),
                    dst: vtable,
                });
                let fun = self.reg_gen.gen(Bytes::B8);
                self.active_block().emit(Hir::Load {
                    base: vtable.into(),
                    index: None,
                    displacement: method.vtable_offset(),
                    dst: fun,
                });

                (
                    fun.into(),
                    method.as_fun().ret().as_ref().map(GetSize::size),
                )
            }
        };

        processed_args.extend(args.iter().map(|e| self.process_expr(e).unwrap()));

        let result = ret.map(|b| self.reg_gen.gen(b));

        self.active_block().emit(Hir::Call {
            fun,
            args: processed_args,
            dst: result,
        });

        result.map(Into::into)
    }

    fn process_expr(&mut self, expr: &Expression<'a>) -> Option<Value<'a>> {
        match expr {
            Expression::Binary { lhs, op, rhs, .. } => {
                self.process_binary_expr(lhs, *op, rhs).into()
            }
            Expression::Call { addr, args, .. } => self.process_call_expr(addr, args),
            Expression::Field {
                class,
                field,
                object,
                ..
            } => self.process_field_expr(class, field, object).into(),
            Expression::Length { array } => {
                let array = self.process_expr(array).unwrap();
                let result = self.reg_gen.gen(Bytes::B4);
                self.active_block().emit(Hir::Load {
                    base: array,
                    index: None,
                    displacement: Context::ARRAY_LENGTH_OFFSET,
                    dst: result,
                });
                Some(result.into())
            }
            Expression::LitBool(b) => Some(Value::ImmB1(i8::from(*b))),
            Expression::LitInt(i) => Some(Value::ImmB4(*i)),
            Expression::LitStr(s) if s.is_empty() => Some(Value::NULL),
            Expression::LitStr(s) => Some(Symbol::Literal(s).into()),
            Expression::Local {
                location,
                value_type,
            } => {
                let size = value_type.size();
                let val = self.get_val(*location, size, self.active_block);
                Some(val)
            }
            Expression::Null(..) => Some(Value::NULL),
            Expression::Slot {
                array,
                index,
                value_type,
            } => self
                .process_slot_expr(array, index, value_type.size())
                .into(),
            Expression::Unary { op, exp } => {
                let inner = self.process_expr(exp).unwrap();
                let size = inner.size();
                let result = self.reg_gen.gen(size);
                match op {
                    UnOp::Neg => self.active_block().emit(Hir::Neg {
                        val: inner,
                        dst: result,
                    }),
                    UnOp::Not => self.active_block().emit(Hir::Bin {
                        lhs: inner,
                        op: BinOp::Xor,
                        rhs: Value::TRUE,
                        dst: result,
                    }),
                }
                Some(result.into())
            }
            Expression::Void => None,
        }
    }

    fn process_while_stmt(&mut self, cond: &Expression<'a>, inner: &[Statement<'a>]) {
        let block_before = self.active_block;

        let block_header_first = self.new_header_block();
        self.active_block().emit(Hir::Jmp {
            dst: block_header_first,
        });

        self.active_block = block_header_first;
        let cond = self.process_expr(cond).unwrap();
        let block_header_last = self.active_block;

        let block_body_first = self.new_simple_block(block_header_last);
        self.active_block = block_body_first;
        for stmt in inner {
            self.process_stmt(stmt);
        }
        self.active_block().emit(Hir::Jmp {
            dst: block_header_first,
        });
        let block_body_last = self.active_block;

        self.block(block_header_first).prev_blocks = vec![block_before, block_body_last];

        let block_after = self.new_simple_block(block_header_last);
        self.block(block_header_last).emit(Hir::CondJmp {
            val: cond,
            dst_if: block_body_first,
            dst_else: block_after,
        });

        self.active_block = block_after;
    }

    fn process_cond_stmt(
        &mut self,
        cond: &Expression<'a>,
        inner_if: &[Statement<'a>],
        inner_else: &[Statement<'a>],
    ) {
        let cond = self.process_expr(cond).unwrap();
        let block_cond_last = self.active_block;

        let block_if_first = self.new_simple_block(block_cond_last);
        self.active_block = block_if_first;
        for stmt in inner_if {
            self.process_stmt(stmt);
        }
        let block_if_last = self.active_block;

        let block_else_first = self.new_simple_block(block_cond_last);
        self.active_block = block_else_first;
        for stmt in inner_else {
            self.process_stmt(stmt);
        }
        let block_else_last = self.active_block;

        self.block(block_cond_last).emit(Hir::CondJmp {
            val: cond,
            dst_if: block_if_first,
            dst_else: block_else_first,
        });

        let block_after = self.new_join_block(block_if_last, block_else_last);
        self.block(block_cond_last).join_hint.replace(block_after);
        self.block(block_if_last)
            .emit(Hir::Jmp { dst: block_after });
        self.block(block_else_last)
            .emit(Hir::Jmp { dst: block_after });

        self.active_block = block_after;
    }

    fn process_assign_stmt(&mut self, ass: &Assignable<'a>, expr: &Expression<'a>) {
        match ass {
            Assignable::Field {
                class,
                field,
                object,
            } => {
                let field = self.context.class(class).field(field);

                let object = self.process_expr(object).unwrap();
                let val = self.process_expr(expr).unwrap();

                self.active_block().emit(Hir::Store {
                    base: object,
                    index: None,
                    displacement: field.offset(),
                    val,
                });
            }
            Assignable::Local { var_idx } => {
                let val = self.process_expr(expr).unwrap();
                self.active_block().define(*var_idx, val);
            }
            Assignable::Slot { array, index } => {
                let array = self.process_expr(array).unwrap();
                let index = self.process_expr(index).unwrap();
                let val = self.process_expr(expr).unwrap();
                let elem_size = expr.get_type().unwrap().size();

                let elems = self.reg_gen.gen(Bytes::B8);
                self.active_block().emit(Hir::Load {
                    base: array,
                    index: None,
                    displacement: Context::ARRAY_ELEMS_OFFSET,
                    dst: elems,
                });

                let imm_offset = match &index {
                    Value::ImmB1(i) => Some(i32::from(*i)),
                    Value::ImmB4(i) => Some(*i),
                    Value::ImmB8(i) => i32::try_from(*i).ok(),
                    _ => None,
                }
                .and_then(|i| i.checked_mul(elem_size.into()));

                match imm_offset {
                    Some(displacement) => {
                        self.active_block().emit(Hir::Store {
                            base: elems.into(),
                            index: None,
                            displacement,
                            val,
                        });
                    }
                    None => {
                        self.active_block().emit(Hir::Store {
                            base: elems.into(),
                            index: Some((index, elem_size)),
                            displacement: 0,
                            val,
                        });
                    }
                }
            }
        }
    }

    pub fn process_stmt(&mut self, stmt: &Statement<'a>) {
        match stmt {
            Statement::Assign {
                destination,
                expression,
            } => self.process_assign_stmt(destination, expression),
            Statement::Cond {
                cond,
                inner_if,
                inner_else,
            } => self.process_cond_stmt(cond, inner_if, inner_else),
            Statement::Expression(e) => {
                self.process_expr(e);
            }
            Statement::Return(e) => {
                let val = self.process_expr(e);
                self.active_block().emit(Hir::Ret { val });
            }
            Statement::While { cond, inner } => self.process_while_stmt(cond, inner),
        }
    }

    fn complete_headers(&mut self) {
        let to_process = self
            .blocks
            .iter()
            .enumerate()
            .filter_map(|(id, block)| block.header_start.then_some(id))
            .map(BlockId)
            .collect::<Vec<_>>();

        for block_id in to_process.into_iter().rev() {
            let block = self.block(block_id);
            let &[block_before, block_body_last] = block.prev_blocks.as_slice() else {
                unreachable!()
            };
            let incomplete = mem::take(&mut block.incomplete_phis);
            for (var, reg) in incomplete {
                let val_before = self.get_val(var, reg.size(), block_before);
                let val_after = self.get_val(var, reg.size(), block_body_last);
                let block = self.block(block_id);
                block.set_var_phi(
                    var,
                    reg,
                    [
                        PhiOption {
                            val: val_before,
                            from: block_before,
                        },
                        PhiOption {
                            val: val_after,
                            from: block_body_last,
                        },
                    ],
                );
                block.try_define(var, reg.into());
                block.incomplete_phis.remove(&var);
            }
        }
    }

    pub fn finish(mut self) -> HirFunction<'a> {
        self.complete_headers();
        let blocks = self
            .blocks
            .into_iter()
            .map(BlockBuilder::finalize_phis)
            .collect::<Vec<_>>();

        opts::Optimizer::new(blocks).run()
    }
}
