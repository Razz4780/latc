mod generator;

use crate::{
    context::{Bytes, GetSize},
    middlend::{BlockId, RelCond},
};
use std::{
    cmp::Ordering,
    fmt::{self, Display, Formatter},
};

pub use generator::AsmProgram;

impl Bytes {
    pub fn asm(self) -> &'static str {
        match self {
            Self::B1 => "byte",
            Self::B4 => "dword",
            Self::B8 => "qword",
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Register {
    RAX,
    RBX,
    RCX,
    RDX,
    RSP,
    RBP,
    RSI,
    RDI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Self::RAX => "rax",
            Self::RBX => "rbx",
            Self::RCX => "rcx",
            Self::RDX => "rdx",
            Self::RSP => "rsp",
            Self::RBP => "rbp",
            Self::RSI => "rsi",
            Self::RDI => "rdi",
            Self::R8 => "r8",
            Self::R9 => "r9",
            Self::R10 => "r10",
            Self::R11 => "r11",
            Self::R12 => "r12",
            Self::R13 => "r13",
            Self::R14 => "r14",
            Self::R15 => "r15",
        };

        f.write_str(as_str)
    }
}

impl Register {
    pub const FUN_PARAMS: [Self; 6] = [
        Self::RDI,
        Self::RSI,
        Self::RDX,
        Self::RCX,
        Self::R8,
        Self::R9,
    ];

    pub fn caller_saved(self) -> bool {
        matches!(
            self,
            Register::RAX
                | Register::RCX
                | Register::RDX
                | Register::RSI
                | Register::RDI
                | Register::R8
                | Register::R9
                | Register::R10
                | Register::R11
        )
    }

    pub fn full(self) -> SizedRegister {
        self.with_size(Bytes::B8)
    }

    pub fn with_size(self, size: Bytes) -> SizedRegister {
        SizedRegister { reg: self, size }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub struct SizedRegister {
    reg: Register,
    size: Bytes,
}

impl SizedRegister {
    pub fn op(self) -> Operand<'static> {
        Operand::Reg(self)
    }
}

impl Display for SizedRegister {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let partial = match (self.reg, self.size) {
            (r, Bytes::B8) => return write!(f, "{}", r),
            (Register::RAX, Bytes::B4) => "eax",
            (Register::RBX, Bytes::B4) => "ebx",
            (Register::RCX, Bytes::B4) => "ecx",
            (Register::RDX, Bytes::B4) => "edx",
            (Register::RSP, Bytes::B4) => "esp",
            (Register::RBP, Bytes::B4) => "ebp",
            (Register::RSI, Bytes::B4) => "esi",
            (Register::RDI, Bytes::B4) => "edi",
            (Register::R8, Bytes::B4) => "r8d",
            (Register::R9, Bytes::B4) => "r9d",
            (Register::R10, Bytes::B4) => "r10d",
            (Register::R11, Bytes::B4) => "r11d",
            (Register::R12, Bytes::B4) => "r12d",
            (Register::R13, Bytes::B4) => "r13d",
            (Register::R14, Bytes::B4) => "r14d",
            (Register::R15, Bytes::B4) => "r15d",
            (Register::RAX, Bytes::B1) => "al",
            (Register::RBX, Bytes::B1) => "bl",
            (Register::RCX, Bytes::B1) => "cl",
            (Register::RDX, Bytes::B1) => "dl",
            (Register::RSP, Bytes::B1) => "spl",
            (Register::RBP, Bytes::B1) => "bpl",
            (Register::RSI, Bytes::B1) => "sil",
            (Register::RDI, Bytes::B1) => "dil",
            (Register::R8, Bytes::B1) => "r8b",
            (Register::R9, Bytes::B1) => "r9b",
            (Register::R10, Bytes::B1) => "r10b",
            (Register::R11, Bytes::B1) => "r11b",
            (Register::R12, Bytes::B1) => "r12b",
            (Register::R13, Bytes::B1) => "r13b",
            (Register::R14, Bytes::B1) => "r14b",
            (Register::R15, Bytes::B1) => "r15b",
        };

        f.write_str(partial)
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Label<'a> {
    Literal(usize),
    VTable(&'a str),
    Function(&'a str),
    Method(&'a str, &'a str),
    Lib(&'a str),
    Block { outer: Box<Label<'a>>, id: BlockId },
}

impl<'a> Display for Label<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(id) => write!(f, "_lit_{}", id),
            Self::VTable(class) => write!(f, "_vtable_{}", class),
            Self::Function(fun) => write!(f, "_fun_{}", fun),
            Self::Method(class, fun) => write!(f, "_class_{}_meth_{}", class, fun),
            Self::Lib(fun) => write!(f, "_lib_{}", fun),
            Self::Block { outer, id } => write!(f, "_block_{}{}", id.0, outer),
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum Operand<'a> {
    Imm(i64, Bytes),
    Reg(SizedRegister),
    Mem {
        base: Box<Operand<'a>>,
        index: Option<(Register, Bytes)>,
        displacement: i32,
        size: Bytes,
    },
    Label(Label<'a>),
}

impl<'a> GetSize for Operand<'a> {
    fn size(&self) -> Bytes {
        match self {
            Self::Imm(_, s) => *s,
            Self::Reg(r) => r.size,
            Self::Mem { size, .. } => *size,
            Self::Label(..) => Bytes::B8,
        }
    }
}

impl<'a> Operand<'a> {
    fn is_imm(&self) -> bool {
        matches!(self, Self::Imm(..))
    }

    fn reg(&self) -> Option<Register> {
        match self {
            Self::Reg(SizedRegister { reg, .. }) => Some(*reg),
            _ => None,
        }
    }

    fn is_mem(&self) -> bool {
        matches!(self, Self::Mem { .. })
    }
}

impl<'a> From<SizedRegister> for Operand<'a> {
    fn from(value: SizedRegister) -> Self {
        Self::Reg(value)
    }
}

impl<'a> Display for Operand<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Imm(i, s) => write!(f, "{} {}", s.asm(), i),
            Self::Reg(r) => r.fmt(f),
            Self::Mem {
                base,
                index,
                displacement,
                size,
            } => {
                write!(f, "{} [{}", size.asm(), base)?;
                if let Some((index, scale)) = index {
                    write!(f, " + {}", index)?;
                    if *scale != Bytes::B1 {
                        write!(f, " * {}", i32::from(*scale))?;
                    }
                }
                match displacement.cmp(&0) {
                    Ordering::Less => write!(f, " - {}", -displacement)?,
                    Ordering::Greater => write!(f, " + {}", displacement)?,
                    Ordering::Equal => {}
                }

                write!(f, "]")
            }
            Self::Label(l) => l.fmt(f),
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Cond {
    L,
    LE,
    G,
    GE,
    E,
    NE,
    Z,
    NZ,
}

impl Cond {
    fn not(self) -> Self {
        match self {
            Self::L => Self::GE,
            Self::LE => Self::G,
            Self::G => Self::LE,
            Self::GE => Self::L,
            Self::E => Self::NE,
            Self::NE => Self::E,
            Self::Z => Self::NZ,
            Self::NZ => Self::Z,
        }
    }

    fn rev(self) -> Self {
        match self {
            Self::L => Self::G,
            Self::LE => Self::GE,
            Self::G => Self::L,
            Self::GE => Self::LE,
            other => other,
        }
    }
}

impl Display for Cond {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let as_str = match self {
            Self::L => "l",
            Self::LE => "le",
            Self::G => "g",
            Self::GE => "ge",
            Self::E => "e",
            Self::NE => "ne",
            Self::Z => "z",
            Self::NZ => "nz",
        };

        f.write_str(as_str)
    }
}

impl From<RelCond> for Cond {
    fn from(value: RelCond) -> Self {
        match value {
            RelCond::EQ => Self::E,
            RelCond::GE => Self::GE,
            RelCond::GTH => Self::G,
            RelCond::LE => Self::LE,
            RelCond::LTH => Self::L,
            RelCond::NEQ => Self::NE,
        }
    }
}

pub enum Inst<'a> {
    Call {
        operand: Operand<'a>,
    },
    Push {
        reg: SizedRegister,
    },
    Pop {
        reg: SizedRegister,
    },
    Mov {
        to: Operand<'a>,
        from: Operand<'a>,
    },
    Sub {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Add {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Xor {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Imul {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Ret,
    Neg {
        operand: Operand<'a>,
    },
    Cmp {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Jmp {
        dst: Label<'a>,
    },
    J {
        cond: Cond,
        dst: Label<'a>,
    },
    Cmov {
        cond: Cond,
        to: Operand<'a>,
        from: Operand<'a>,
    },
    Test {
        lhs: Operand<'a>,
        rhs: Operand<'a>,
    },
    Idiv {
        operand: Operand<'a>,
    },
    Cdq,
    Lea {
        dst: Operand<'a>,
        mem: Operand<'a>,
    },
}

impl<'a> Display for Inst<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Call { operand } => write!(f, "call {}", operand),
            Self::Push { reg } => write!(f, "push {}", reg),
            Self::Pop { reg } => write!(f, "pop {}", reg),
            Self::Mov { to, from } => write!(f, "mov {}, {}", to, from),
            Self::Sub { lhs, rhs } => write!(f, "sub {}, {}", lhs, rhs),
            Self::Add { lhs, rhs } => write!(f, "add {}, {}", lhs, rhs),
            Self::Xor { lhs, rhs } => write!(f, "xor {}, {}", lhs, rhs),
            Self::Imul { lhs, rhs } => write!(f, "imul {}, {}", lhs, rhs),
            Self::Ret => f.write_str("ret"),
            Self::Neg { operand } => write!(f, "neg {}", operand),
            Self::Cmp { lhs, rhs } => write!(f, "cmp {}, {}", lhs, rhs),
            Self::Jmp { dst } => write!(f, "jmp {}", dst),
            Self::J { cond, dst } => write!(f, "j{} {}", cond, dst),
            Self::Cmov { cond, to, from } => write!(f, "cmov{} {}, {}", cond, to, from),
            Self::Test { lhs, rhs } => write!(f, "test {}, {}", lhs, rhs),
            Self::Idiv { operand } => write!(f, "idiv {}", operand),
            Self::Cdq => f.write_str("cdq"),
            Self::Lea { dst, mem } => write!(f, "lea {}, {}", dst, mem),
        }
    }
}
