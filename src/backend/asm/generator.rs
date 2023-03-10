use super::{Cond, Inst, Label, Operand, Register, SizedRegister};
use crate::{
    backend::{
        intervals::{Interval, IntervalBuilder},
        regalloc::{RegAllocator, Slot, SCRATCH_1, SCRATCH_2},
    },
    context::{Bytes, Context, GetSize},
    frontend::FunctionId,
    middlend::{BinOp, BlockId, Hir, HirFunction, HirLoc, RelCond, Symbol, VReg, VRegId, Value},
};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::{self, Display, Formatter},
};

pub struct SymbolResolver<'a, 'b> {
    literals: HashMap<&'a str, Label<'a>>,
    context: &'b Context<'a>,
}

impl<'a, 'b> SymbolResolver<'a, 'b> {
    pub fn new(context: &'b Context<'a>) -> Self {
        Self {
            context,
            literals: Default::default(),
        }
    }

    fn resolve(&mut self, symbol: &Symbol<'a>) -> Operand<'a> {
        match symbol {
            Symbol::AddStrings => Operand::Label(Label::Lib("addStrings")),
            Symbol::CmpStrings => Operand::Label(Label::Lib("cmpStrings")),
            Symbol::Function(name) => Operand::Label(Label::Function(name)),
            Symbol::NewArray => Operand::Label(Label::Lib("newArray")),
            Symbol::NewObject => Operand::Label(Label::Lib("newObject")),
            Symbol::VTable(class) if self.context.class(class).unwrap().methods().is_empty() => {
                Operand::Imm(0, Bytes::B8)
            }
            Symbol::VTable(class) => Operand::Label(Label::VTable(class)),
            Symbol::Literal(lit) if lit.is_empty() => Operand::Imm(0, Bytes::B8),
            Symbol::Literal(lit) => {
                let next_id = self.literals.len();
                match self.literals.entry(lit) {
                    Entry::Occupied(e) => Operand::Label(e.get().clone()),
                    Entry::Vacant(e) => {
                        let label = Label::Literal(next_id);
                        e.insert(label.clone());
                        Operand::Label(label)
                    }
                }
            }
        }
    }

    pub fn literals(self) -> Vec<(Label<'a>, &'a str)> {
        self.literals
            .into_iter()
            .map(|(lit, label)| (label, lit))
            .collect()
    }
}

struct StackLayout {
    preserved_regs: Vec<Register>,
    stack_slots: usize,
    is_caller: bool,
    has_stack_params: bool,

    saved_regs: Vec<Register>,
    stack_adjustment: usize,
}

impl StackLayout {
    fn new(
        fun: &HirFunction<'_>,
        allocation: &HashMap<VRegId, (Slot, Interval)>,
        params: usize,
    ) -> Self {
        let preserved_regs = allocation
            .values()
            .filter_map(|v| match v.0 {
                Slot::Reg(r) if !r.caller_saved() => Some(r),
                _ => None,
            })
            .collect::<HashSet<_>>()
            .into_iter()
            .collect::<Vec<_>>();

        let stack_slots = allocation
            .values()
            .filter_map(|v| match v.0 {
                Slot::Stack(i) => Some(i + 1),
                _ => None,
            })
            .max()
            .unwrap_or(0);

        let is_caller = fun
            .blocks()
            .iter()
            .flatten()
            .any(|hir| matches!(hir, Hir::Call { .. }));

        let has_stack_params = params > Register::FUN_PARAMS.len();

        Self {
            preserved_regs,
            stack_slots,
            is_caller,
            has_stack_params,

            saved_regs: Default::default(),
            stack_adjustment: 0,
        }
    }

    fn rbp_pushed(&self) -> bool {
        !self.preserved_regs.is_empty()
            || self.stack_slots > 0
            || self.is_caller
            || self.has_stack_params
    }

    fn base_stack_entries(&self) -> usize {
        usize::from(self.rbp_pushed()) + self.preserved_regs.len() + self.stack_slots
    }

    fn prologue(&self) -> Vec<Inst<'static>> {
        let mut insts: Vec<Inst<'static>> = Default::default();

        if self.rbp_pushed() {
            insts.push(Inst::Push {
                reg: Register::RBP.with_size(Bytes::B8),
            });
            insts.push(Inst::Mov {
                to: Register::RBP.with_size(Bytes::B8).into(),
                from: Register::RSP.with_size(Bytes::B8).into(),
            });
        }

        for reg in &self.preserved_regs {
            insts.push(Inst::Push {
                reg: reg.with_size(Bytes::B8),
            });
        }

        if self.stack_slots > 0 {
            let mem = i64::try_from(self.stack_slots).unwrap() * 8;
            insts.push(Inst::Sub {
                lhs: Register::RSP.with_size(Bytes::B8).into(),
                rhs: Operand::Imm(mem, Bytes::B8),
            });
        }

        insts
    }

    fn epilogue(&self) -> Vec<Inst<'static>> {
        let mut insts: Vec<Inst<'static>> = Default::default();

        if self.stack_slots > 0 {
            let mem = i64::try_from(self.stack_slots).unwrap() * 8;
            insts.push(Inst::Add {
                lhs: Register::RSP.with_size(Bytes::B8).into(),
                rhs: Operand::Imm(mem, Bytes::B8),
            });
        }

        for reg in self
            .preserved_regs
            .iter()
            .rev()
            .copied()
            .collect::<Vec<_>>()
        {
            insts.push(Inst::Pop {
                reg: reg.with_size(Bytes::B8),
            });
        }

        if self.rbp_pushed() {
            insts.push(Inst::Pop {
                reg: Register::RBP.with_size(Bytes::B8),
            });
        }

        insts
    }

    fn resolve_slot(&self, slot: Slot, size: Bytes) -> Operand<'static> {
        match slot {
            Slot::Param(i) => Operand::Mem {
                base: Box::new(Register::RBP.full().op()),
                index: None,
                displacement: (i32::try_from(i).unwrap() + 2) * 8,
                size,
            },
            Slot::Reg(r) => r.with_size(size).op(),
            Slot::Stack(i) => {
                let saved_regs = i32::try_from(self.preserved_regs.len()).unwrap();
                Operand::Mem {
                    base: Box::new(Register::RBP.full().op()),
                    index: None,
                    displacement: (1 + saved_regs + i32::try_from(i).unwrap()) * -8,
                    size,
                }
            }
        }
    }

    fn save_regs(&mut self, regs: Vec<Register>) -> Vec<Inst<'static>> {
        let mut insts = Vec::with_capacity(regs.len());
        for reg in &regs {
            insts.push(Inst::Push {
                reg: reg.with_size(Bytes::B8),
            });
        }
        self.saved_regs = regs;

        insts
    }

    fn restore_regs(&mut self) -> Vec<Inst<'static>> {
        let mut insts = Vec::with_capacity(self.saved_regs.len());
        for reg in self.saved_regs.drain(..).rev() {
            insts.push(Inst::Pop {
                reg: reg.with_size(Bytes::B8),
            });
        }
        insts
    }

    fn prepare_stack(&mut self, fun_params: usize) -> Option<Inst<'static>> {
        let stack_params = fun_params.saturating_sub(Register::FUN_PARAMS.len());
        let stack_entries = self.base_stack_entries() + self.saved_regs.len() + stack_params;

        self.stack_adjustment = if stack_entries % 2 == 1 {
            stack_params * 8
        } else {
            (stack_params + 1) * 8
        };

        if self.stack_adjustment > 0 {
            Some(Inst::Sub {
                lhs: Register::RSP.with_size(Bytes::B8).into(),
                rhs: Operand::Imm(self.stack_adjustment.try_into().unwrap(), Bytes::B8),
            })
        } else {
            None
        }
    }

    fn restore_stack(&mut self) -> Option<Inst<'static>> {
        let inst = if self.stack_adjustment > 0 {
            Some(Inst::Add {
                lhs: Register::RSP.with_size(Bytes::B8).into(),
                rhs: Operand::Imm(self.stack_adjustment.try_into().unwrap(), Bytes::B8),
            })
        } else {
            None
        };

        self.stack_adjustment = 0;

        inst
    }

    fn prepare_call<'a>(
        &self,
        args: Vec<Operand<'a>>,
        call_opd: Operand<'a>,
    ) -> (Vec<Inst<'a>>, Operand<'a>) {
        let saved_reg_addr = |reg: Register| -> Operand<'static> {
            let saved_idx = self
                .saved_regs
                .iter()
                .copied()
                .enumerate()
                .find(|(_, s)| *s == reg)
                .unwrap()
                .0;
            Operand::Mem {
                base: Box::new(Register::RBP.with_size(Bytes::B8).into()),
                index: None,
                displacement: i32::try_from(
                    saved_idx + 1 + self.preserved_regs.len() + self.stack_slots,
                )
                .unwrap()
                    * -8,
                size: Bytes::B8,
            }
        };

        let mut insts: Vec<Inst<'a>> = Default::default();

        for (idx, arg) in (0..args.len()).zip(args).rev() {
            if idx < Register::FUN_PARAMS.len() {
                let reg = Register::FUN_PARAMS[idx];
                match arg.reg() {
                    Some(r) if r == reg => {}
                    Some(r) if Register::FUN_PARAMS.contains(&r) => {
                        let from = saved_reg_addr(r);
                        insts.push(Inst::Mov {
                            to: reg.full().into(),
                            from,
                        });
                    }
                    _ => insts.push(Inst::Mov {
                        to: reg.with_size(arg.size()).into(),
                        from: arg,
                    }),
                }
            } else {
                let stack_idx = idx - Register::FUN_PARAMS.len();

                let from = if arg.is_mem() {
                    let scratch = Operand::Reg(SCRATCH_1.with_size(arg.size()));
                    insts.push(Inst::Mov {
                        to: scratch.clone(),
                        from: arg,
                    });
                    scratch
                } else {
                    arg
                };

                let to = Operand::Mem {
                    base: Box::new(Register::RSP.with_size(Bytes::B8).into()),
                    index: None,
                    displacement: i32::try_from(stack_idx).unwrap() * 8,
                    size: from.size(),
                };

                insts.push(Inst::Mov { to, from });
            }
        }

        let call_opd = match call_opd {
            Operand::Reg(r) if Register::FUN_PARAMS.contains(&r.reg) => {
                let from = saved_reg_addr(r.reg);
                let scratch = SCRATCH_1.full().op();
                insts.push(Inst::Mov {
                    to: scratch.clone(),
                    from,
                });
                scratch
            }
            opd if opd.is_mem() => {
                let scratch = SCRATCH_1.with_size(opd.size()).op();
                insts.push(Inst::Mov {
                    to: scratch.clone(),
                    from: opd,
                });
                scratch
            }
            opd => opd,
        };

        (insts, call_opd)
    }
}

pub struct AsmFunctionBuilder<'a, 'b> {
    name: Label<'a>,

    allocation: HashMap<VRegId, (Slot, Interval)>,
    new_slot_begins: Vec<(HirLoc, VRegId, Slot)>,
    old_slot_ends: Vec<(HirLoc, Slot)>,
    used_slots: HashMap<Slot, VRegId>,

    blocks: Vec<(Label<'a>, Vec<Inst<'a>>)>,
    current_loc: HirLoc,
    hir_fun: &'b HirFunction<'a>,
    symbols: SymbolResolver<'a, 'b>,

    stack_layout: StackLayout,
}

impl<'a, 'b> AsmFunctionBuilder<'a, 'b> {
    pub fn new(
        name: FunctionId<'a>,
        params: Vec<Bytes>,
        hir_fun: &'b HirFunction<'a>,
        symbols: SymbolResolver<'a, 'b>,
    ) -> Self {
        let name = match name {
            FunctionId::Global { name } => Label::Function(name),
            FunctionId::Method { name, class } => Label::Method(class, name),
        };

        let intervals = IntervalBuilder::new(hir_fun).run();
        let allocation = RegAllocator::new(intervals, params.len())
            .run()
            .into_iter()
            .map(|(it, slot)| (it.vreg(), (slot, it)))
            .collect::<HashMap<_, _>>();

        let mut new_slot_begins = allocation
            .values()
            .map(|(slot, it)| (*it.begin(), it.vreg(), *slot))
            .collect::<Vec<_>>();
        new_slot_begins.sort_unstable_by(|u1, u2| u2.0.cmp(&u1.0));

        let mut old_slot_ends = allocation
            .values()
            .map(|(slot, it)| (*it.end(), *slot))
            .collect::<Vec<_>>();
        old_slot_ends.sort_unstable_by(|u1, u2| u2.0.cmp(&u1.0));

        let stack_layout = StackLayout::new(hir_fun, &allocation, params.len());

        Self {
            name,

            allocation,
            new_slot_begins,
            old_slot_ends,

            used_slots: Default::default(),

            blocks: Default::default(),

            current_loc: HirLoc {
                block: BlockId(0),
                hir_idx: 0,
            },
            hir_fun,
            symbols,

            stack_layout,
        }
    }

    fn process_cmp(&mut self, lhs: &Value<'a>, rhs: &Value<'a>, cond: RelCond, dst: &VReg) {
        let lhs = self.make_operand(lhs);
        let rhs = self.make_operand(rhs);
        let cond = Cond::from(cond);

        let (lhs, rhs, cond) = if lhs.is_imm() {
            (rhs, lhs, cond.rev())
        } else if lhs.is_mem() {
            (lhs, self.assert_not_mem(rhs, SCRATCH_1), cond)
        } else {
            (lhs, rhs, cond)
        };
        self.emit(Inst::Cmp { lhs, rhs });

        let next_loc = HirLoc {
            block: self.current_loc.block,
            hir_idx: self.current_loc.hir_idx + 1,
        };
        let next_hir = &self.hir_fun.block(next_loc.block)[next_loc.hir_idx];
        if let Hir::CondJmp {
            val,
            dst_if,
            dst_else,
        } = next_hir
        {
            if val == &Value::VReg(*dst)
                && self.allocation.get(&dst.id).unwrap().1.end() == &next_loc
            {
                self.consume_slot_ends();
                self.current_loc = next_loc;
                self.consume_slot_begins();

                if *dst_if == self.current_loc.block + 1 {
                    self.emit_phi_stores(*dst_else, Some(cond.not()));
                    self.emit(Inst::J {
                        cond: cond.not(),
                        dst: self.block_label(*dst_else),
                    });
                    self.emit_phi_stores(*dst_if, None);
                } else if *dst_else == self.current_loc.block + 1 {
                    self.emit_phi_stores(*dst_if, Some(cond));
                    self.emit(Inst::J {
                        cond,
                        dst: self.block_label(*dst_if),
                    });
                    self.emit_phi_stores(*dst_else, None);
                } else {
                    self.emit_phi_stores(*dst_if, Some(cond));
                    self.emit(Inst::J {
                        cond,
                        dst: self.block_label(*dst_if),
                    });
                    self.emit_phi_stores(*dst_else, None);
                    self.emit(Inst::Jmp {
                        dst: self.block_label(*dst_else),
                    });
                }

                return;
            }
        }

        let to = self.make_operand(&Value::VReg(*dst));
        let scratch_1 = SCRATCH_1.with_size(Bytes::B4).op();
        let scratch_2 = SCRATCH_2.with_size(Bytes::B4).op();
        self.emit(Inst::Mov {
            to: scratch_1.clone(),
            from: Operand::Imm(0, scratch_1.size()),
        });
        self.emit(Inst::Mov {
            to: scratch_2.clone(),
            from: Operand::Imm(1, scratch_2.size()),
        });
        self.emit(Inst::Cmov {
            cond,
            to: scratch_1.clone(),
            from: scratch_2.clone(),
        });
        self.emit(Inst::Mov {
            to,
            from: SCRATCH_1.with_size(Bytes::B1).op(),
        });
    }

    fn process_idiv(&mut self, lhs: &Value<'a>, rhs: &Value<'a>, dst: &VReg, res_in: Register) {
        let dst_id = dst.id;
        let dst = self.make_operand(&Value::VReg(*dst));
        let lhs = self.make_operand(lhs);
        let rhs = self.make_operand(rhs);

        let rax_saved = match self.used_slots.get(&Slot::Reg(Register::RAX)).copied() {
            Some(vr) if vr == dst_id => None,
            None => None,
            Some(_) => {
                let scratch = SCRATCH_1.full().op();
                self.emit(Inst::Mov {
                    to: scratch.clone(),
                    from: Register::RAX.with_size(Bytes::B8).into(),
                });
                Some(scratch)
            }
        };

        let rdx_saved = match self.used_slots.get(&Slot::Reg(Register::RDX)).copied() {
            Some(vr) if vr == dst_id => None,
            None => None,
            Some(_) => {
                let scratch = SCRATCH_2.full().op();
                self.emit(Inst::Mov {
                    to: scratch.clone(),
                    from: Register::RDX.with_size(Bytes::B8).into(),
                });
                Some(scratch)
            }
        };

        if lhs.reg() != Some(Register::RAX) {
            self.emit(Inst::Mov {
                to: Register::RAX.with_size(lhs.size()).into(),
                from: lhs.clone(),
            });
        }
        self.emit(Inst::Cdq);
        let idiv_opd = match &rhs {
            Operand::Mem { .. } => rhs,
            Operand::Reg(SizedRegister {
                reg: Register::RAX, ..
            }) => rax_saved.clone().unwrap(),
            Operand::Reg(SizedRegister {
                reg: Register::RDX, ..
            }) => rdx_saved.clone().unwrap(),
            Operand::Reg(..) => rhs,
            _ if rax_saved.is_none() => {
                let scratch = SCRATCH_1.with_size(rhs.size()).op();
                self.emit(Inst::Mov {
                    to: scratch.clone(),
                    from: rhs,
                });
                scratch
            }
            _ if rdx_saved.is_none() => {
                let scratch = SCRATCH_2.with_size(rhs.size()).op();
                self.emit(Inst::Mov {
                    to: scratch.clone(),
                    from: rhs,
                });
                scratch
            }
            _ => {
                self.emit(Inst::Mov {
                    to: dst.clone(),
                    from: rhs,
                });
                dst.clone()
            }
        };
        self.emit(Inst::Idiv { operand: idiv_opd });

        if dst.reg() != Some(res_in) {
            let dst_size = dst.size();
            self.emit(Inst::Mov {
                to: dst,
                from: res_in.with_size(dst_size).into(),
            });
        }

        if let Some(from) = rdx_saved {
            self.emit(Inst::Mov {
                to: Register::RDX.with_size(Bytes::B8).into(),
                from,
            });
        }
        if let Some(from) = rax_saved {
            self.emit(Inst::Mov {
                to: Register::RAX.with_size(Bytes::B8).into(),
                from,
            });
        }
    }

    fn emit_phi_stores(&mut self, block: BlockId, cond: Option<Cond>) {
        let moves = self
            .hir_fun
            .block(block)
            .iter()
            .map_while(|hir| match hir {
                Hir::Phi { dst, options } => Some([(dst, &options[0]), (dst, &options[1])]),
                _ => None,
            })
            .flatten()
            .filter_map(|(dst, opt)| {
                if opt.from == self.current_loc.block {
                    Some((
                        self.make_operand(&opt.val),
                        self.make_operand(&Value::VReg(*dst)),
                    ))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();

        for (src, dst) in moves {
            let src = if (src.is_mem() && dst.is_mem()) || (src.is_imm() && cond.is_some()) {
                let scratch = SCRATCH_1.with_size(src.size()).op();
                self.emit(Inst::Mov {
                    to: scratch.clone(),
                    from: src,
                });
                scratch
            } else {
                src
            };

            self.emit(Inst::Mov { to: dst, from: src });
        }
    }

    fn assert_not_mem(&mut self, operand: Operand<'a>, scratch: Register) -> Operand<'a> {
        if operand.is_mem() {
            let scratch = Operand::Reg(scratch.with_size(operand.size()));
            self.emit(Inst::Mov {
                to: scratch.clone(),
                from: operand,
            });
            scratch
        } else {
            operand
        }
    }

    fn assert_reg(&mut self, operand: Operand<'a>, scratch: Register) -> Operand<'a> {
        if operand.reg().is_none() {
            let scratch = Operand::Reg(scratch.with_size(operand.size()));
            self.emit(Inst::Mov {
                to: scratch.clone(),
                from: operand,
            });
            scratch
        } else {
            operand
        }
    }

    fn process_call(&mut self, fun: &Value<'a>, args: &[Value<'a>], dst: Option<&VReg>) {
        let to_save = self
            .used_slots
            .iter()
            .filter(|(_, vreg)| Some(**vreg) != dst.map(|v| v.id))
            .filter_map(|(slot, _)| slot.reg())
            .filter(|r| r.caller_saved())
            .collect::<Vec<_>>();
        self.stack_layout
            .save_regs(to_save)
            .into_iter()
            .for_each(|i| self.emit(i));

        self.stack_layout
            .prepare_stack(args.len())
            .into_iter()
            .for_each(|i| self.emit(i));
        let args = args
            .iter()
            .map(|a| self.make_operand(a))
            .collect::<Vec<_>>();
        let call_opd = self.make_operand(fun);
        let (insts, call_opd) = self.stack_layout.prepare_call(args, call_opd);
        insts.into_iter().for_each(|i| self.emit(i));

        self.emit(Inst::Call { operand: call_opd });
        if let Some(dst) = dst {
            let operand = self.make_operand(&Value::VReg(*dst));
            if operand.reg() != Some(Register::RAX) {
                self.emit(Inst::Mov {
                    to: operand,
                    from: Register::RAX.with_size(dst.size()).into(),
                });
            }
        }

        self.stack_layout
            .restore_stack()
            .into_iter()
            .for_each(|i| self.emit(i));

        self.stack_layout
            .restore_regs()
            .into_iter()
            .for_each(|i| self.emit(i));
    }

    fn get_addr_in_reg(
        &mut self,
        base: &Value<'a>,
        index: Option<&(Value<'a>, Bytes)>,
    ) -> Operand<'a> {
        let base = self.make_operand(base);
        let base = self.assert_reg(base, SCRATCH_1);

        match index {
            Some((index, scale)) => {
                let index = self.make_operand(index);
                let index = self.assert_reg(index, SCRATCH_2).reg().unwrap();
                let addr = SCRATCH_1.full().op();
                self.emit(Inst::Lea {
                    dst: addr.clone(),
                    mem: Operand::Mem {
                        base: base.into(),
                        index: Some((index, *scale)),
                        displacement: 0,
                        size: Bytes::B8,
                    },
                });
                addr
            }
            None => base,
        }
    }

    fn process_hir(&mut self, hir: &Hir<'a>) {
        match hir {
            Hir::Ret { val: None } => {
                self.stack_layout
                    .epilogue()
                    .into_iter()
                    .for_each(|i| self.emit(i));
                self.emit(Inst::Ret);
            }
            Hir::Ret { val: Some(val) } => {
                let operand = self.make_operand(val);
                if operand.reg() != Some(Register::RAX) {
                    self.emit(Inst::Mov {
                        to: Register::RAX.with_size(val.size()).into(),
                        from: operand,
                    });
                }
                self.stack_layout
                    .epilogue()
                    .into_iter()
                    .for_each(|i| self.emit(i));
                self.emit(Inst::Ret);
            }
            Hir::Neg { val, dst } => {
                let to = self.make_operand(&Value::VReg(*dst));
                let from = self.make_operand(val);

                if to.is_mem() && from.is_mem() {
                    let from = self.assert_not_mem(from, SCRATCH_1);
                    self.emit(Inst::Neg {
                        operand: from.clone(),
                    });
                    self.emit(Inst::Mov { to, from });
                } else {
                    self.emit(Inst::Mov {
                        to: to.clone(),
                        from,
                    });
                    self.emit(Inst::Neg { operand: to });
                }
            }
            Hir::Bin {
                lhs,
                op: BinOp::Add,
                rhs,
                dst,
            } => {
                let dst = self.make_operand(&Value::VReg(*dst));
                let lhs = self.make_operand(lhs);
                let rhs = self.make_operand(rhs);

                if dst.is_mem() && (lhs.is_mem() || rhs.is_mem()) {
                    let scratch = SCRATCH_1.with_size(lhs.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Add {
                        lhs: scratch.clone(),
                        rhs,
                    });
                    self.emit(Inst::Mov {
                        to: dst,
                        from: scratch,
                    });
                } else {
                    self.emit(Inst::Mov {
                        to: dst.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Add { lhs: dst, rhs });
                }
            }
            Hir::Bin {
                lhs,
                op: BinOp::Mul,
                rhs,
                dst,
            } => {
                let dst = self.make_operand(&Value::VReg(*dst));
                let lhs = self.make_operand(lhs);
                let rhs = self.make_operand(rhs);

                if dst.is_mem() {
                    let scratch = SCRATCH_1.with_size(lhs.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Imul {
                        lhs: scratch.clone(),
                        rhs,
                    });
                    self.emit(Inst::Mov {
                        to: dst,
                        from: scratch,
                    });
                } else {
                    self.emit(Inst::Mov {
                        to: dst.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Imul { lhs: dst, rhs });
                }
            }
            Hir::Bin {
                lhs,
                op: BinOp::Sub,
                rhs,
                dst,
            } => {
                let dst = self.make_operand(&Value::VReg(*dst));
                let lhs = self.make_operand(lhs);
                let rhs = self.make_operand(rhs);

                if dst.is_mem() && (lhs.is_mem() || rhs.is_mem()) {
                    let scratch = SCRATCH_1.with_size(lhs.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Sub {
                        lhs: scratch.clone(),
                        rhs,
                    });
                    self.emit(Inst::Mov {
                        to: dst,
                        from: scratch,
                    });
                } else {
                    self.emit(Inst::Mov {
                        to: dst.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Sub { lhs: dst, rhs });
                }
            }
            Hir::Bin {
                lhs,
                op: BinOp::Xor,
                rhs,
                dst,
            } => {
                let dst = self.make_operand(&Value::VReg(*dst));
                let lhs = self.make_operand(lhs);
                let rhs = self.make_operand(rhs);

                if dst.is_mem() && (lhs.is_mem() || rhs.is_mem()) {
                    let scratch = SCRATCH_1.with_size(lhs.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Xor {
                        lhs: scratch.clone(),
                        rhs,
                    });
                    self.emit(Inst::Mov {
                        to: dst,
                        from: scratch,
                    });
                } else {
                    self.emit(Inst::Mov {
                        to: dst.clone(),
                        from: lhs,
                    });
                    self.emit(Inst::Xor { lhs: dst, rhs });
                }
            }
            Hir::Bin {
                lhs,
                op: BinOp::Mod,
                rhs,
                dst,
            } => self.process_idiv(lhs, rhs, dst, Register::RDX),
            Hir::Bin {
                lhs,
                op: BinOp::Div,
                rhs,
                dst,
            } => self.process_idiv(lhs, rhs, dst, Register::RAX),
            Hir::Call { fun, args, dst } => self.process_call(fun, args, dst.as_ref()),
            Hir::Cmp {
                lhs,
                rhs,
                cond,
                dst,
            } => self.process_cmp(lhs, rhs, *cond, dst),
            Hir::CondJmp {
                val,
                dst_if,
                dst_else,
            } => {
                let operand = self.make_operand(val);
                let operand = if operand.reg().is_none() {
                    let scratch = SCRATCH_1.with_size(operand.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: operand,
                    });
                    scratch
                } else {
                    operand
                };
                self.emit(Inst::Test {
                    lhs: operand.clone(),
                    rhs: operand,
                });
                if *dst_if == self.current_loc.block + 1 {
                    self.emit_phi_stores(*dst_else, Some(Cond::Z));
                    self.emit(Inst::J {
                        cond: Cond::Z,
                        dst: self.block_label(*dst_else),
                    });
                    self.emit_phi_stores(*dst_if, None);
                } else if *dst_else == self.current_loc.block + 1 {
                    self.emit_phi_stores(*dst_if, Some(Cond::NZ));
                    self.emit(Inst::J {
                        cond: Cond::NZ,
                        dst: self.block_label(*dst_if),
                    });
                    self.emit_phi_stores(*dst_else, None);
                } else {
                    self.emit_phi_stores(*dst_if, Some(Cond::NZ));
                    self.emit(Inst::J {
                        cond: Cond::NZ,
                        dst: self.block_label(*dst_if),
                    });
                    self.emit_phi_stores(*dst_else, None);
                    self.emit(Inst::Jmp {
                        dst: self.block_label(*dst_else),
                    });
                }
            }
            Hir::Jmp { dst } if *dst == self.current_loc.block + 1 => {
                self.emit_phi_stores(*dst, None)
            }
            Hir::Jmp { dst } => {
                self.emit_phi_stores(*dst, None);
                let dst = self.block_label(*dst);
                self.emit(Inst::Jmp { dst });
            }
            Hir::Load {
                base,
                index,
                displacement,
                dst,
            } => {
                let addr = self.get_addr_in_reg(base, index.as_ref());
                let dst = self.make_operand(&Value::VReg(*dst));
                let (dst, true_dst) = if dst.is_mem() {
                    (SCRATCH_2.with_size(dst.size()).op(), Some(dst))
                } else {
                    (dst, None)
                };
                self.emit(Inst::Mov {
                    from: Operand::Mem {
                        base: addr.into(),
                        index: None,
                        displacement: *displacement,
                        size: dst.size(),
                    },
                    to: dst.clone(),
                });

                if let Some(true_dst) = true_dst {
                    self.emit(Inst::Mov {
                        from: dst,
                        to: true_dst,
                    });
                }
            }
            Hir::Store {
                base,
                index,
                displacement,
                val,
            } => {
                let addr = self.get_addr_in_reg(base, index.as_ref());
                let val = self.make_operand(val);
                let val = if val.is_mem() {
                    let scratch = SCRATCH_2.with_size(val.size()).op();
                    self.emit(Inst::Mov {
                        to: scratch.clone(),
                        from: val,
                    });
                    scratch
                } else {
                    val
                };
                self.emit(Inst::Mov {
                    to: Operand::Mem {
                        base: addr.into(),
                        index: None,
                        displacement: *displacement,
                        size: val.size(),
                    },
                    from: val,
                });
            }

            Hir::Phi { .. } => {}
            Hir::Removed => {}
        }
    }

    fn make_operand(&mut self, value: &Value<'a>) -> Operand<'a> {
        match value {
            Value::ImmB1(i) => Operand::Imm(i64::from(*i), Bytes::B1),
            Value::ImmB4(i) => Operand::Imm(i64::from(*i), Bytes::B4),
            Value::ImmB8(i) => Operand::Imm(*i, Bytes::B8),
            Value::Symbol(s) => self.symbols.resolve(s),
            Value::VReg(vreg) => {
                let slot = self
                    .allocation
                    .get(&vreg.id)
                    .unwrap_or_else(|| panic!("{:?} {:?}", vreg, self.current_loc))
                    .0;
                self.stack_layout.resolve_slot(slot, vreg.size())
            }
        }
    }

    fn block_label(&self, id: BlockId) -> Label<'a> {
        Label::Block {
            outer: self.name.clone().into(),
            id,
        }
    }

    fn new_block(&mut self, id: BlockId) {
        self.blocks.push((self.block_label(id), Default::default()));
    }

    fn emit(&mut self, inst: Inst<'a>) {
        self.blocks.last_mut().unwrap().1.push(inst);
    }

    fn consume_slot_begins(&mut self) {
        while let Some(new) = self.new_slot_begins.pop() {
            if new.0 <= self.current_loc {
                self.used_slots.insert(new.2, new.1);
            } else {
                self.new_slot_begins.push(new);
                return;
            }
        }
    }

    fn consume_slot_ends(&mut self) {
        while let Some(old) = self.old_slot_ends.pop() {
            if old.0 <= self.current_loc {
                self.used_slots.remove(&old.1);
            } else {
                self.old_slot_ends.push(old);
                return;
            }
        }
    }

    pub fn run(mut self) -> (AsmFunction<'a>, SymbolResolver<'a, 'b>) {
        loop {
            let Some(block) = self.hir_fun.blocks().get(self.current_loc.block.0) else {
                break;
            };
            let Some(hir) = block.get(self.current_loc.hir_idx) else {
                self.current_loc.block = self.current_loc.block + 1;
                self.current_loc.hir_idx = 0;
                continue;
            };

            self.consume_slot_begins();

            if self.current_loc.hir_idx == 0 {
                self.new_block(self.current_loc.block);
                if self.current_loc.block.0 == 0 {
                    self.stack_layout
                        .prologue()
                        .into_iter()
                        .for_each(|i| self.emit(i));
                }
            }
            self.process_hir(hir);

            self.consume_slot_ends();

            self.current_loc.hir_idx += 1;
        }

        (
            AsmFunction {
                label: self.name,
                blocks: self.blocks,
            },
            self.symbols,
        )
    }
}

pub struct AsmFunction<'a> {
    label: Label<'a>,
    blocks: Vec<(Label<'a>, Vec<Inst<'a>>)>,
}

impl<'a> Display for AsmFunction<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for (block_label, insts) in &self.blocks {
            writeln!(f, "{}:", block_label)?;
            for inst in insts {
                writeln!(f, "\t{}", inst)?;
            }
        }

        Ok(())
    }
}
