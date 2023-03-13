mod generator;

use crate::{
    ast,
    context::{Bytes, Context, FunctionId, GetSize},
    frontend::CheckedProgram,
};
use generator::HirGenerator;
use std::{
    collections::HashMap,
    fmt::{self, Debug, Formatter},
    ops::{Add, Sub},
};

/// High-level intermediate representation of a correct Latte function.
pub struct HirFunction<'a> {
    blocks: Vec<Vec<Hir<'a>>>,
    cfg: Vec<Edges>,
}

impl<'a> HirFunction<'a> {
    /// Returns the basic block with the given id.
    /// Panics if the block does not exist.
    pub fn block(&self, id: BlockId) -> &[Hir<'a>] {
        self.blocks[id.0].as_slice()
    }

    /// Returns all basic blocks in this function.
    pub fn blocks(&self) -> &[Vec<Hir<'a>>] {
        self.blocks.as_slice()
    }

    /// Returns parents of the basic block with the given id.
    /// Panics if the block does not exist.
    pub fn parents_of(&self, id: BlockId) -> &[BlockId] {
        self.cfg[id.0].parents()
    }

    /// Returns children of the basic block with the given id.
    /// Panics if the block does not exist.
    pub fn children_of(&self, id: BlockId) -> &[BlockId] {
        self.cfg[id.0].children()
    }
}

/// High-level intermediate representation of a correct Latte program.
pub struct HirProgram<'a> {
    context: Context<'a>,
    functions: HashMap<FunctionId<'a>, HirFunction<'a>>,
}

impl<'a> HirProgram<'a> {
    /// Creates a new instance of this struct.
    /// Should never panic, as [`CheckedProgram`] represents a valid Latte program.
    pub fn new(program: CheckedProgram<'a>) -> Self {
        let mut functions: HashMap<FunctionId<'a>, HirFunction<'a>> = Default::default();

        for (name, stmts) in program.defs() {
            let ctx = program.ctx().fn_context(name).unwrap();
            let mut generator = HirGenerator::new(ctx);
            for stmt in stmts {
                generator.process_stmt(stmt);
            }
            let fun = generator.finish();

            functions.insert(*name, fun);
        }

        Self {
            context: program.into(),
            functions,
        }
    }

    /// Returns the [`Context`] for this program.
    pub fn context(&self) -> &Context<'a> {
        &self.context
    }

    /// Returns all [`HirFunction`]s in this program.
    pub fn functions(&self) -> &HashMap<FunctionId<'a>, HirFunction<'a>> {
        &self.functions
    }
}

/// An id of a basic block inside [`HirFunction`].
#[derive(Default, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub usize);

impl Add<usize> for BlockId {
    type Output = BlockId;

    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

impl Sub<usize> for BlockId {
    type Output = BlockId;

    fn sub(self, rhs: usize) -> Self::Output {
        Self(self.0 - rhs)
    }
}

impl Debug for BlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "L{}", self.0)
    }
}

/// An id of a virtual register used by [`Hir`] code.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VRegId(pub usize);

impl Debug for VRegId {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

/// Virtual register used by [`Hir`] code.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VReg {
    pub id: VRegId,
    pub size: Bytes,
}

impl Debug for VReg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} {:?}", self.size, self.id)
    }
}

impl GetSize for VReg {
    fn size(&self) -> Bytes {
        self.size
    }
}

/// Symbols used to identify globally accessible data.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Symbol<'a> {
    Literal(&'a str),
    /// Hidden Latte builtin.
    AddStrings,
    /// Hidden Latte builtin.
    CmpStrings,
    /// Hidden Latte builtin.
    NewArray,
    /// Hidden Latte builtin.
    NewObject,
    Function(&'a str),
    VTable(&'a str),
}

/// A value used by the [`Hir`] code.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Value<'a> {
    VReg(VReg),
    ImmB8(i64),
    ImmB4(i32),
    ImmB1(i8),
    Symbol(Symbol<'a>),
}

impl<'a> Debug for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::VReg(r) => write!(f, "{:?}", r),
            Self::ImmB8(i) => write!(f, "{:?} {:?}", self.size(), i),
            Self::ImmB4(i) => write!(f, "{:?} {:?}", self.size(), i),
            Self::ImmB1(i) => write!(f, "{:?} {:?}", self.size(), i),
            Self::Symbol(s) => write!(f, "{:?} {:?}", self.size(), s),
        }
    }
}

impl<'a> GetSize for Value<'a> {
    fn size(&self) -> Bytes {
        match self {
            Self::VReg(r) => r.size(),
            Self::ImmB8(..) => Bytes::B8,
            Self::ImmB4(..) => Bytes::B4,
            Self::ImmB1(..) => Bytes::B1,
            Self::Symbol(..) => Bytes::B8,
        }
    }
}

impl<'a> Value<'a> {
    pub const NULL: Self = Self::ImmB8(0);
    pub const TRUE: Self = Self::ImmB1(1);
    pub const FALSE: Self = Self::ImmB1(0);

    pub fn is_imm(&self) -> bool {
        matches!(self, Self::ImmB1(..) | Self::ImmB4(..) | Self::ImmB8(..))
    }
}

impl<'a> From<VReg> for Value<'a> {
    fn from(value: VReg) -> Self {
        Self::VReg(value)
    }
}

impl<'a> From<i64> for Value<'a> {
    fn from(value: i64) -> Self {
        Self::ImmB8(value)
    }
}

impl<'a> From<i32> for Value<'a> {
    fn from(value: i32) -> Self {
        Self::ImmB4(value)
    }
}

impl<'a> From<i8> for Value<'a> {
    fn from(value: i8) -> Self {
        Self::ImmB1(value)
    }
}

impl<'a> From<Symbol<'a>> for Value<'a> {
    fn from(value: Symbol<'a>) -> Self {
        Self::Symbol(value)
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Xor,
}

impl Debug for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Self::Add => "add",
            Self::Sub => "sub",
            Self::Mul => "mul",
            Self::Div => "div",
            Self::Mod => "mod",
            Self::Xor => "xor",
        };

        f.write_str(as_str)
    }
}

pub struct ConvertionNotPossible;

impl TryFrom<ast::BinOp> for BinOp {
    type Error = ConvertionNotPossible;

    fn try_from(value: ast::BinOp) -> Result<Self, Self::Error> {
        match value {
            ast::BinOp::Add => Ok(Self::Add),
            ast::BinOp::Sub => Ok(Self::Sub),
            ast::BinOp::Mul => Ok(Self::Mul),
            ast::BinOp::Div => Ok(Self::Div),
            ast::BinOp::Mod => Ok(Self::Mod),
            _ => Err(ConvertionNotPossible),
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RelCond {
    LTH,
    LE,
    GTH,
    GE,
    EQ,
    NEQ,
}

impl TryFrom<ast::BinOp> for RelCond {
    type Error = ConvertionNotPossible;

    fn try_from(value: ast::BinOp) -> Result<Self, Self::Error> {
        match value {
            ast::BinOp::LTH => Ok(Self::LTH),
            ast::BinOp::LE => Ok(Self::LE),
            ast::BinOp::GTH => Ok(Self::GTH),
            ast::BinOp::GE => Ok(Self::GE),
            ast::BinOp::EQ => Ok(Self::EQ),
            ast::BinOp::NEQ => Ok(Self::NEQ),
            _ => Err(ConvertionNotPossible),
        }
    }
}

impl RelCond {
    fn as_cc(self) -> &'static str {
        match self {
            Self::LTH => "l",
            Self::LE => "le",
            Self::GTH => "g",
            Self::GE => "ge",
            Self::EQ => "e",
            Self::NEQ => "ne",
        }
    }
}

#[derive(PartialEq, Eq, Hash)]
pub struct PhiOption<'a> {
    pub val: Value<'a>,
    pub from: BlockId,
}

impl<'a> Debug for PhiOption<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[{:?}, {:?}]", self.from, self.val)
    }
}

/// A high-level intermediate representation.
/// Modeled after LLVM.
pub enum Hir<'a> {
    Load {
        base: Value<'a>,
        index: Option<(Value<'a>, Bytes)>,
        displacement: i32,
        dst: VReg,
    },
    Store {
        base: Value<'a>,
        index: Option<(Value<'a>, Bytes)>,
        displacement: i32,
        val: Value<'a>,
    },
    Bin {
        lhs: Value<'a>,
        op: BinOp,
        rhs: Value<'a>,
        dst: VReg,
    },
    Neg {
        val: Value<'a>,
        dst: VReg,
    },
    Cmp {
        lhs: Value<'a>,
        rhs: Value<'a>,
        cond: RelCond,
        dst: VReg,
    },
    CondJmp {
        val: Value<'a>,
        dst_if: BlockId,
        dst_else: BlockId,
    },
    Jmp {
        dst: BlockId,
    },
    Phi {
        dst: VReg,
        options: [PhiOption<'a>; 2],
    },
    Call {
        fun: Value<'a>,
        args: Vec<Value<'a>>,
        dst: Option<VReg>,
    },
    Ret {
        val: Option<Value<'a>>,
    },
    Removed,
}

impl<'a> Debug for Hir<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bin { lhs, op, rhs, dst } => {
                write!(f, "{:?} = {:?} {:?}, {:?}", dst, op, lhs, rhs)
            }
            Self::Call {
                fun,
                args,
                dst: Some(dst),
            } => write!(f, "{:?} = call {:?}{:?}", dst, fun, args),
            Self::Call {
                fun,
                args,
                dst: None,
            } => write!(f, "call {:?}{:?}", fun, args),
            Self::Cmp {
                lhs,
                rhs,
                dst,
                cond,
            } => write!(f, "{:?} = cmp{} {:?}, {:?}", dst, cond.as_cc(), lhs, rhs),
            Self::CondJmp {
                val,
                dst_if,
                dst_else,
            } => write!(f, "jmp {:?} {:?}, {:?}", val, dst_if, dst_else),
            Self::Jmp { dst } => write!(f, "jmp {:?}", dst),
            Self::Load {
                base,
                index: Some((index, scale)),
                displacement,
                dst,
            } => write!(
                f,
                "load {:?}, [{:?} + ({:?} * {:?}) + {}]",
                dst,
                base,
                index,
                i32::from(*scale),
                displacement
            ),
            Self::Load {
                base,
                index: None,
                displacement,
                dst,
            } => write!(f, "load {:?}, [{:?} + {}]", dst, base, displacement),
            Self::Neg { val, dst } => write!(f, "{:?} = neg {:?}", dst, val),
            Self::Phi { dst, options } => write!(f, "{:?} = phi {:?}", dst, options),
            Self::Ret { val: Some(val) } => write!(f, "ret {:?}", val),
            Self::Ret { val: None } => write!(f, "ret"),
            Self::Store {
                base,
                index: Some((index, scale)),
                displacement,
                val,
            } => write!(
                f,
                "store {:?}, [{:?} + ({:?} * {:?}) + {}]",
                val,
                base,
                index,
                i32::from(*scale),
                displacement
            ),
            Self::Store {
                base,
                index: None,
                displacement,
                val,
            } => write!(f, "store {:?}, [{:?} + {}]", val, base, displacement),
            Self::Removed => f.write_str("<removed>"),
        }
    }
}

/// A node in the control flow graph, corresponds to a single basic block.
#[derive(Default, Clone, Debug)]
struct Edges {
    children: Vec<BlockId>,
    parents: Vec<BlockId>,
}

impl Edges {
    /// Returns the children of this basic block.
    fn children(&self) -> &[BlockId] {
        self.children.as_slice()
    }

    /// Returns the parents of this basic block.
    fn parents(&self) -> &[BlockId] {
        self.parents.as_slice()
    }
}

/// A location of a [`Hir`] instruction inside a [`HirFunction`].
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct HirLoc {
    /// Id of the basic block containing this instruction.
    pub block: BlockId,
    /// Index of this instruction inside the basic block.
    pub hir_idx: usize,
}

impl Debug for HirLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}:{}", self.block, self.hir_idx)
    }
}
