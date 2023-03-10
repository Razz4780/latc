use super::*;
use std::{
    cmp::{self, Ordering},
    collections::{HashMap, HashSet},
    num::Wrapping,
    ops::{Add, BitXor, Div, Mul, Rem, Sub},
};

impl RelCond {
    fn eval<T: Ord + Eq>(self, lhs: T, rhs: T) -> bool {
        match self {
            Self::EQ => lhs == rhs,
            Self::NEQ => lhs != rhs,
            Self::GTH => lhs > rhs,
            Self::GE => lhs >= rhs,
            Self::LTH => lhs < rhs,
            Self::LE => lhs <= rhs,
        }
    }

    fn rev(self) -> Self {
        match self {
            Self::GTH => Self::LTH,
            Self::GE => Self::LE,
            Self::LE => Self::GE,
            Self::LTH => Self::GTH,
            other => other,
        }
    }
}

impl BinOp {
    fn eval<T>(self, lhs: T, rhs: T) -> Option<T>
    where
        Wrapping<T>: Add<Wrapping<T>, Output = Wrapping<T>>
            + Sub<Output = Wrapping<T>>
            + Mul<Output = Wrapping<T>>
            + Div<Output = Wrapping<T>>,
        T: Eq + From<i8> + Rem<Output = T> + BitXor<Output = T>,
    {
        match self {
            Self::Add => Some((Wrapping(lhs) + Wrapping(rhs)).0),
            Self::Sub => Some((Wrapping(lhs) - Wrapping(rhs)).0),
            Self::Mul => Some((Wrapping(lhs) * Wrapping(rhs)).0),
            Self::Div if rhs == 0.into() => None,
            Self::Div => Some((Wrapping(lhs) / Wrapping(rhs)).0),
            Self::Mod if rhs == 0.into() => None,
            Self::Mod => Some(lhs % rhs),
            Self::Xor => Some(lhs ^ rhs),
        }
    }
}

impl<'a> Value<'a> {
    pub fn as_vreg(&self) -> Option<&VReg> {
        match self {
            Self::VReg(r) => Some(r),
            _ => None,
        }
    }

    fn eval_cmp(lhs: &Self, cond: RelCond, rhs: &Self) -> Option<Self> {
        let result = match (lhs, rhs) {
            (Self::ImmB1(lhs), Self::ImmB1(rhs)) => cond.eval(lhs, rhs),
            (Self::ImmB4(lhs), Self::ImmB4(rhs)) => cond.eval(lhs, rhs),
            (Self::ImmB8(lhs), Self::ImmB8(rhs)) => cond.eval(lhs, rhs),
            _ => return None,
        };

        Some(Self::ImmB1(i8::from(result)))
    }

    fn eval_neg(val: &Self) -> Option<Self> {
        match val {
            Self::ImmB1(i) => Some(Self::ImmB1(i.wrapping_neg())),
            Self::ImmB4(i) => Some(Self::ImmB4(i.wrapping_neg())),
            Self::ImmB8(i) => Some(Self::ImmB8(i.wrapping_neg())),
            _ => None,
        }
    }

    fn eval_bin(lhs: &Self, op: BinOp, rhs: &Self) -> Option<Value<'a>> {
        match (lhs, rhs, op) {
            (Self::ImmB1(lhs), Self::ImmB1(rhs), _) => op.eval(*lhs, *rhs).map(Self::ImmB1),
            (Self::ImmB4(lhs), Self::ImmB4(rhs), _) => op.eval(*lhs, *rhs).map(Self::ImmB4),
            (Self::ImmB8(lhs), Self::ImmB8(rhs), _) => op.eval(*lhs, *rhs).map(Self::ImmB8),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum PureExpr<'a> {
    Bin {
        lhs: Value<'a>,
        op: BinOp,
        rhs: Value<'a>,
    },
    Neg {
        val: Value<'a>,
    },
    Cmp {
        lhs: Value<'a>,
        cond: RelCond,
        rhs: Value<'a>,
    },
}

impl<'a> PureExpr<'a> {
    pub fn inspect_used<F: FnMut(&VReg)>(&self, mut f: F) {
        match self {
            Self::Bin { lhs, rhs, .. } => {
                if let Value::VReg(r) = lhs {
                    f(r);
                }
                if let Value::VReg(r) = rhs {
                    f(r);
                }
            }
            Self::Neg {
                val: Value::VReg(r),
                ..
            } => f(r),
            Self::Cmp { lhs, rhs, .. } => {
                if let Value::VReg(r) = lhs {
                    f(r);
                }
                if let Value::VReg(r) = rhs {
                    f(r);
                }
            }
            _ => {}
        }
    }
}

impl<'a> Hir<'a> {
    pub fn get_defined(&self) -> Option<&VReg> {
        match self {
            Self::Load { dst, .. } => Some(dst),
            Self::Bin { dst, .. } => Some(dst),
            Self::Neg { dst, .. } => Some(dst),
            Self::Cmp { dst, .. } => Some(dst),
            Self::Phi { dst, .. } => Some(dst),
            Self::Call { dst: Some(dst), .. } => Some(dst),
            _ => None,
        }
    }

    pub fn inspect_used<F: FnMut(&VReg)>(&self, mut f: F) {
        match self {
            Self::Load { base, index, .. } => {
                if let Value::VReg(r) = base {
                    f(r);
                }
                if let Some((Value::VReg(r), _)) = index {
                    f(r);
                }
            }
            Self::Store {
                base, index, val, ..
            } => {
                if let Value::VReg(r) = base {
                    f(r);
                }
                if let Some((Value::VReg(r), _)) = index {
                    f(r);
                }
                if let Value::VReg(r) = val {
                    f(r);
                }
            }
            Self::Bin { lhs, rhs, .. } => {
                if let Value::VReg(r) = lhs {
                    f(r);
                }
                if let Value::VReg(r) = rhs {
                    f(r);
                }
            }
            Self::Neg {
                val: Value::VReg(r),
                ..
            } => f(r),
            Self::CondJmp {
                val: Value::VReg(r),
                ..
            } => f(r),
            Self::Cmp { lhs, rhs, .. } => {
                if let Value::VReg(r) = lhs {
                    f(r);
                }
                if let Value::VReg(r) = rhs {
                    f(r);
                }
            }
            Self::Phi { options, .. } => options.iter().filter_map(|o| o.val.as_vreg()).for_each(f),
            Self::Call { fun, args, .. } => {
                if let Value::VReg(r) = fun {
                    f(r);
                }
                args.iter().filter_map(Value::as_vreg).for_each(f)
            }
            Self::Ret {
                val: Some(Value::VReg(r)),
            } => {
                f(r);
            }
            _ => {}
        }
    }

    fn for_each_used<F: FnMut(&mut Value<'a>)>(&mut self, mut f: F) {
        match self {
            Self::Load { base, index, .. } => {
                f(base);
                if let Some((idx, _)) = index {
                    f(idx);
                }
            }
            Self::Store {
                base, index, val, ..
            } => {
                f(base);
                if let Some((idx, _)) = index {
                    f(idx);
                }
                f(val);
            }
            Self::Bin { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Self::Neg { val, .. } => f(val),
            Self::CondJmp { val, .. } => f(val),
            Self::Cmp { lhs, rhs, .. } => {
                f(lhs);
                f(rhs);
            }
            Self::Phi { options, .. } => {
                options.iter_mut().for_each(|o| f(&mut o.val));
            }
            Self::Call { fun, args, .. } => {
                f(fun);
                args.iter_mut().for_each(f);
            }
            Self::Ret { val: Some(val) } => {
                f(val);
            }
            _ => {}
        }
    }

    fn eval(&self) -> Option<Value<'a>> {
        match self {
            Self::Bin { lhs, op, rhs, .. } => Value::eval_bin(lhs, *op, rhs),
            Self::Neg { val, .. } => Value::eval_neg(val),
            Self::Cmp { lhs, rhs, cond, .. } => Value::eval_cmp(lhs, *cond, rhs),
            Self::Phi {
                options: [o1, o2], ..
            } => {
                if o1.val.as_vreg().is_none() && o1.val == o2.val {
                    Some(o1.val)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn pure_expr(&self) -> Option<PureExpr<'a>> {
        match self {
            Self::Bin { lhs, op, rhs, .. } => {
                if matches!(op, BinOp::Add | BinOp::Mul | BinOp::Xor) {
                    Some(PureExpr::Bin {
                        lhs: *cmp::min(lhs, rhs),
                        op: *op,
                        rhs: *cmp::max(lhs, rhs),
                    })
                } else {
                    Some(PureExpr::Bin {
                        lhs: *lhs,
                        op: *op,
                        rhs: *rhs,
                    })
                }
            }
            Self::Neg { val, .. } => Some(PureExpr::Neg { val: *val }),
            Self::Cmp { lhs, cond, rhs, .. } => {
                let cond = if lhs <= rhs { *cond } else { cond.rev() };

                Some(PureExpr::Cmp {
                    lhs: *cmp::min(lhs, rhs),
                    cond,
                    rhs: *cmp::max(lhs, rhs),
                })
            }
            _ => None,
        }
    }
}

type RegDefMap<'a> = HashMap<VRegId, HirLoc>;
type RegUseMap<'a> = HashMap<VRegId, HashSet<HirLoc>>;

impl<'a> HirFunction<'a> {
    fn get_last_dominated_blocks(&self) -> Vec<Option<BlockId>> {
        let mut result = vec![None; self.blocks().len()];

        for id in (0..self.blocks().len()).rev().map(BlockId) {
            let dom = match *self.children_of(id) {
                [c1, c2] => {
                    let l1 = result[c1.0].unwrap_or(c1);
                    let l1_children = self.children_of(l1);
                    let l2 = result[c2.0].unwrap_or(c2);
                    let l2_children = self.children_of(l2);
                    if l1_children.len() == 1 && l1_children == l2_children {
                        Some(result[l1_children[0].0].unwrap_or(l1_children[0]))
                    } else {
                        Some(cmp::max(l1, l2))
                    }
                }
                [c] if c > id && self.parents_of(c).len() == 1 => cmp::max(result[c.0], Some(c)),
                _ => None,
            };

            result[id.0] = dom;
        }

        result
    }

    fn get_def_use_chain(&self) -> (RegDefMap<'a>, RegUseMap<'a>) {
        let mut reg_defs: RegDefMap<'a> = Default::default();
        let mut reg_uses: RegUseMap<'a> = Default::default();

        for (block_id, hirs) in self.blocks().iter().enumerate() {
            let block = BlockId(block_id);
            for (hir_idx, hir) in hirs.iter().enumerate() {
                if let Some(r) = hir.get_defined() {
                    reg_defs.insert(r.id, HirLoc { block, hir_idx });
                }
                hir.inspect_used(|r| {
                    reg_uses
                        .entry(r.id)
                        .or_default()
                        .insert(HirLoc { block, hir_idx });
                });
            }
        }

        (reg_defs, reg_uses)
    }

    fn rebuild_cfg(&mut self) {
        self.cfg.clear();
        self.cfg.resize(self.blocks.len(), Default::default());

        let mut seen: HashSet<BlockId> = Default::default();
        let mut worklist = vec![BlockId(0)];
        while let Some(id) = worklist.pop() {
            if !seen.insert(id) {
                continue;
            }

            match self.blocks[id.0].last() {
                Some(Hir::Jmp { dst }) => {
                    self.cfg[id.0].children.push(*dst);
                    self.cfg[dst.0].parents.push(id);
                    worklist.push(*dst);
                }
                Some(Hir::CondJmp {
                    dst_if, dst_else, ..
                }) => {
                    self.cfg[id.0].children.push(*dst_if);
                    self.cfg[dst_if.0].parents.push(id);
                    worklist.push(*dst_if);

                    self.cfg[id.0].children.push(*dst_else);
                    self.cfg[dst_else.0].parents.push(id);
                    worklist.push(*dst_else);
                }
                _ => {}
            };
        }
    }

    fn get_hir(&self, loc: &HirLoc) -> &Hir<'a> {
        &self.blocks[loc.block.0][loc.hir_idx]
    }

    fn get_hir_mut(&mut self, loc: &HirLoc) -> &mut Hir<'a> {
        &mut self.blocks[loc.block.0][loc.hir_idx]
    }

    fn compact(&mut self) {
        let mut old_blocks: Vec<Vec<Hir<'a>>> = Default::default();
        mem::swap(&mut self.blocks, &mut old_blocks);

        let mut renames = (0..old_blocks.len()).map(BlockId).collect::<Vec<_>>();
        for (id, mut block) in old_blocks.into_iter().enumerate() {
            block.retain(|hir| !matches!(hir, Hir::Removed));
            if block.is_empty() {
                continue;
            }

            renames[id] = BlockId(self.blocks.len());
            self.blocks.push(block);
        }

        self.blocks.iter_mut().flatten().for_each(|hir| match hir {
            Hir::Jmp { dst } => *dst = renames[dst.0],
            Hir::CondJmp {
                dst_if, dst_else, ..
            } => {
                *dst_if = renames[dst_if.0];
                *dst_else = renames[dst_else.0];
            }
            Hir::Phi { options, .. } => {
                options[0].from = renames[options[0].from.0];
                options[1].from = renames[options[1].from.0];
            }
            _ => {}
        });

        self.rebuild_cfg();
    }
}

pub struct Optimizer<'a> {
    fun: HirFunction<'a>,
    reg_defs: RegDefMap<'a>,
    reg_uses: RegUseMap<'a>,
}

impl<'a> Optimizer<'a> {
    pub fn new(mut blocks: Vec<Vec<Hir<'a>>>) -> Self {
        let mut seen: HashSet<BlockId> = Default::default();
        let mut worklist = vec![BlockId(0)];
        while let Some(id) = worklist.pop() {
            if !seen.insert(id) {
                continue;
            }

            let block = &mut blocks[id.0];

            let real_size = 1 + block
                .iter()
                .take_while(|hir| match hir {
                    Hir::Jmp { dst } => {
                        worklist.push(*dst);
                        false
                    }
                    Hir::CondJmp {
                        dst_if, dst_else, ..
                    } => {
                        worklist.push(*dst_if);
                        worklist.push(*dst_else);
                        false
                    }
                    Hir::Ret { .. } => false,
                    Hir::Call {
                        fun: Value::Symbol(Symbol::Function("error")),
                        ..
                    } => false,
                    _ => true,
                })
                .count();

            block.truncate(real_size);
        }

        for (id, block) in blocks.iter_mut().enumerate() {
            if !seen.contains(&BlockId(id)) {
                *block = Default::default();
            }
        }

        let mut fun = HirFunction {
            blocks,
            cfg: Default::default(),
        };
        fun.rebuild_cfg();
        let (reg_defs, reg_uses) = fun.get_def_use_chain();

        Self {
            fun,
            reg_defs,
            reg_uses,
        }
    }

    fn sparse_propagate(&mut self) {
        let mut worklist = self.reg_defs.values().copied().collect::<Vec<_>>();

        while let Some(loc) = worklist.pop() {
            let hir = &mut self.fun.blocks[loc.block.0][loc.hir_idx];
            let simplified = match (hir.eval(), &hir) {
                (Some(constant), _) => constant,
                (None, Hir::Phi { dst, options }) => {
                    if options[0].val.as_vreg() == Some(dst) {
                        options[1].val
                    } else if options[1].val.as_vreg() == Some(dst) {
                        options[0].val
                    } else if !self.fun.cfg[loc.block.0].parents.contains(&options[0].from) {
                        options[1].val
                    } else if !self.fun.cfg[loc.block.0].parents.contains(&options[1].from) {
                        options[0].val
                    } else {
                        continue;
                    }
                }
                (None, _) => continue,
            };

            hir.inspect_used(|r| {
                self.reg_uses.get_mut(&r.id).unwrap().remove(&loc);
            });
            let defined = *hir.get_defined().unwrap();
            self.reg_defs.remove(&defined.id);

            *hir = Hir::Removed;

            for loc in self.reg_uses.remove(&defined.id).unwrap_or_default() {
                self.fun.get_hir_mut(&loc).for_each_used(|v| {
                    if v.as_vreg() == Some(&defined) {
                        *v = simplified;
                    }
                });
                if let Some(r) = simplified.as_vreg() {
                    self.reg_uses.entry(r.id).or_default().insert(loc);
                }
                worklist.push(loc);
            }
        }
    }

    fn run_gcse(&mut self) {
        let last_doms = self.fun.get_last_dominated_blocks();
        let dominates = move |loc_1: &HirLoc, loc_2: &HirLoc| match loc_1.block.cmp(&loc_2.block) {
            Ordering::Greater => false,
            Ordering::Equal => loc_2.hir_idx > loc_1.hir_idx,
            Ordering::Less => Some(loc_2.block) <= last_doms[loc_1.block.0],
        };

        let mut worklist: Vec<HirLoc> = self.reg_defs.values().copied().collect();

        let mut candidates_buffer: Vec<HirLoc> = Default::default();
        while let Some(old_loc) = worklist.pop() {
            let Some(old_expr) = self.fun.get_hir(&old_loc).pure_expr() else { continue };
            old_expr.inspect_used(|r| {
                for use_loc in self.reg_uses.get(&r.id).unwrap() {
                    candidates_buffer.push(*use_loc);
                }
            });

            for new_loc in candidates_buffer.drain(..) {
                if !dominates(&new_loc, &old_loc) {
                    continue;
                }
                let Some(new_expr) = self.fun.get_hir(&new_loc).pure_expr() else {
                    continue
                };
                if new_expr != old_expr {
                    continue;
                }

                let old_hir = self.fun.get_hir_mut(&old_loc);
                let old_reg = *old_hir.get_defined().unwrap();

                old_hir.inspect_used(|r| {
                    self.reg_uses.get_mut(&r.id).unwrap().remove(&old_loc);
                });
                self.reg_defs.remove(&old_reg.id);
                *old_hir = Hir::Removed;

                let new_reg = *self.fun.get_hir(&new_loc).get_defined().unwrap();
                let stale_uses = self.reg_uses.remove(&old_reg.id).unwrap_or_default();
                for use_loc in stale_uses {
                    self.reg_uses.entry(new_reg.id).or_default().insert(use_loc);
                    self.fun.get_hir_mut(&use_loc).for_each_used(|v| {
                        if let Value::VReg(r) = v {
                            if r.id == old_reg.id {
                                r.id = new_reg.id;
                            }
                        }
                    });
                    worklist.push(use_loc);
                }

                break;
            }
        }
    }

    fn remove_dead_hirs(&mut self) {
        let mut worklist = self
            .reg_defs
            .keys()
            .copied()
            .filter(|reg| {
                self.reg_uses
                    .get(reg)
                    .map(HashSet::is_empty)
                    .unwrap_or(true)
            })
            .collect::<Vec<_>>();

        while let Some(unused) = worklist.pop() {
            let Some(loc) = self.reg_defs.remove(&unused) else { continue };
            let hir = self.fun.get_hir_mut(&loc);

            if let Hir::Call { dst, .. } = hir {
                *dst = None;
            } else {
                hir.inspect_used(|r| {
                    let uses = self.reg_uses.get_mut(&r.id).unwrap();
                    uses.remove(&loc);
                    if uses.is_empty() {
                        worklist.push(r.id);
                    }
                });
                *hir = Hir::Removed;
            }
        }
    }

    fn remove_dead_blocks(&mut self) {
        let mut seen: HashSet<BlockId> = Default::default();
        let mut worklist = vec![BlockId(0)];
        while let Some(id) = worklist.pop() {
            if !seen.insert(id) {
                continue;
            }

            let hir = self.fun.blocks[id.0].last_mut().unwrap();
            match hir {
                Hir::Jmp { dst } => worklist.push(*dst),
                Hir::CondJmp {
                    val: Value::FALSE,
                    dst_else,
                    ..
                } => {
                    worklist.push(*dst_else);
                    *hir = Hir::Jmp { dst: *dst_else };
                }
                Hir::CondJmp {
                    val: Value::TRUE,
                    dst_if,
                    ..
                } => {
                    worklist.push(*dst_if);
                    *hir = Hir::Jmp { dst: *dst_if };
                }
                Hir::CondJmp {
                    dst_if, dst_else, ..
                } => {
                    worklist.push(*dst_if);
                    worklist.push(*dst_else);
                }
                _ => {}
            }
        }

        for (id, block) in self.fun.blocks.iter_mut().enumerate() {
            let block_id = BlockId(id);
            if !seen.contains(&block_id) {
                for (hir_idx, hir) in block.iter().enumerate() {
                    if let Some(defined) = hir.get_defined() {
                        self.reg_defs.remove(&defined.id);
                    }
                    hir.inspect_used(|r| {
                        self.reg_uses.get_mut(&r.id).unwrap().remove(&HirLoc {
                            block: block_id,
                            hir_idx,
                        });
                    })
                }
                *block = Default::default();
            }
        }

        self.fun.rebuild_cfg();
    }

    pub fn run(mut self) -> HirFunction<'a> {
        self.sparse_propagate();
        self.remove_dead_blocks();
        self.sparse_propagate();
        self.remove_dead_hirs();
        self.run_gcse();

        self.fun.compact();

        self.fun
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::Bytes;

    #[test]
    fn hir_eval() {
        let mut gen = RegGenerator::default();

        assert_eq!(
            Hir::Bin {
                lhs: Value::ImmB1(1),
                op: BinOp::Add,
                rhs: Value::ImmB1(1),
                dst: gen.gen(Bytes::B1)
            }
            .eval(),
            Some(Value::ImmB1(2))
        );
        assert_eq!(
            Hir::Bin {
                lhs: Value::ImmB4(2),
                op: BinOp::Mul,
                rhs: Value::ImmB4(4),
                dst: gen.gen(Bytes::B4)
            }
            .eval(),
            Some(Value::ImmB4(8)),
        );
        assert_eq!(
            Hir::Bin {
                lhs: Value::ImmB8(1),
                op: BinOp::Mod,
                rhs: Value::ImmB8(0),
                dst: gen.gen(Bytes::B8)
            }
            .eval(),
            None,
        );
        assert_eq!(
            Hir::Bin {
                lhs: Value::ImmB8(1),
                op: BinOp::Mod,
                rhs: Value::ImmB8(0),
                dst: gen.gen(Bytes::B8)
            }
            .eval(),
            None,
        );
        assert_eq!(
            Hir::Bin {
                lhs: Value::ImmB4(0),
                op: BinOp::Add,
                rhs: Value::VReg(gen.gen(Bytes::B4)),
                dst: gen.gen(Bytes::B4)
            }
            .eval(),
            None,
        );
        assert_eq!(
            Hir::Call {
                fun: Value::Symbol(Symbol::Function("whatever")),
                args: vec![],
                dst: gen.gen(Bytes::B8).into()
            }
            .eval(),
            None
        );
        assert_eq!(
            Hir::Cmp {
                lhs: Value::ImmB1(1),
                rhs: Value::ImmB1(3),
                cond: RelCond::EQ,
                dst: gen.gen(Bytes::B1)
            }
            .eval(),
            Some(Value::ImmB1(0)),
        );
        let reg = gen.gen(Bytes::B8);
        assert_eq!(
            Hir::Phi {
                dst: reg,
                options: [
                    PhiOption {
                        val: Value::ImmB8(42),
                        from: BlockId(0)
                    },
                    PhiOption {
                        val: Value::VReg(reg),
                        from: BlockId(1),
                    }
                ]
            }
            .eval(),
            None,
        )
    }
}
