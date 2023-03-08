use super::{asm::Register, intervals::Interval};
use std::{
    cmp::{Ord, Ordering, PartialOrd, Reverse},
    collections::BinaryHeap,
    fmt::{self, Debug, Formatter},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Slot {
    Reg(Register),
    Stack(usize),
    Param(usize),
}

impl Slot {
    pub fn reg(self) -> Option<Register> {
        match self {
            Self::Reg(reg) => Some(reg),
            _ => None,
        }
    }
}

impl Debug for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(r) => write!(f, "{}", r),
            Self::Stack(i) => write!(f, "s{}", i),
            Self::Param(i) => write!(f, "p{}", i),
        }
    }
}

pub struct RegAllocator {
    live: Vec<Interval>,
    active: Vec<(Interval, Register)>,
    handled: Vec<(Interval, Slot)>,

    free_regs: BinaryHeap<RegWithPreference>,

    slots: Vec<Slot>,
    next_stack_slot: usize,
}

impl RegAllocator {
    pub fn new(mut intervals: Vec<Interval>, params: usize) -> Self {
        let mut handled: Vec<(Interval, Slot)> = Default::default();
        let mut i = 0;
        while i < intervals.len() {
            let vr = intervals[i].vreg().0;

            if vr >= params {
                i += 1;
                continue;
            }

            if vr < Register::FUN_PARAMS.len() {
                handled.push((
                    intervals.swap_remove(i),
                    Slot::Reg(Register::FUN_PARAMS[vr]),
                ));
            } else {
                handled.push((
                    intervals.swap_remove(i),
                    Slot::Param(vr - Register::FUN_PARAMS.len()),
                ));
            }
        }

        let free_regs = ALLOCATABLE
            .into_iter()
            .filter(|reg| {
                handled
                    .iter()
                    .find(|elem| elem.1 == Slot::Reg(*reg))
                    .is_none()
            })
            .map(RegWithPreference)
            .collect();

        Self {
            live: intervals,
            active: Default::default(),
            handled,

            free_regs,

            slots: Default::default(),
            next_stack_slot: 0,
        }
    }

    fn get_slot(&mut self) -> Slot {
        self.slots.pop().unwrap_or_else(|| {
            let slot = self.next_stack_slot;
            self.next_stack_slot += 1;
            Slot::Stack(slot)
        })
    }

    fn add_to_active(&mut self, it: Interval, reg: Register) {
        let pos = self
            .active
            .iter()
            .take_while(|prev| prev.0.end() <= it.end())
            .count();
        self.active.insert(pos, (it, reg));
    }

    fn expire_old_intervals(&mut self, it: &Interval) {
        while let Some((prev, _)) = self.active.first() {
            if prev.end() >= it.begin() {
                return;
            }

            let (prev, reg) = self.active.remove(0);
            self.free_regs.push(RegWithPreference(reg));
            self.handled.push((prev, Slot::Reg(reg)));
        }
    }

    fn spill_at(&mut self, it: Interval) {
        let last = self.active.pop();
        match last {
            Some((last, reg)) if last.end() > it.end() => {
                let slot = self.get_slot();
                self.handled.push((last, slot));
                self.add_to_active(it, reg);
            }
            Some(other) => {
                self.active.push(other);
                let slot = self.get_slot();
                self.handled.push((it, slot));
            }
            None => unreachable!(),
        }
    }

    pub fn handle(&mut self, it: Interval) {
        self.expire_old_intervals(&it);

        match self.free_regs.pop() {
            Some(reg) => self.add_to_active(it, reg.0),
            None => self.spill_at(it),
        }
    }

    pub fn run(mut self) -> Vec<(Interval, Slot)> {
        self.live.sort_unstable_by_key(|it| Reverse(*it.begin()));

        while let Some(next) = self.live.pop() {
            self.handle(next);
        }

        self.active.into_iter().for_each(|(it, reg)| {
            self.handled.push((it, Slot::Reg(reg)));
        });

        self.handled
    }
}

const ALLOCATABLE: [Register; 12] = [
    Register::RAX,
    Register::RBX,
    Register::RCX,
    Register::RDX,
    Register::RSI,
    Register::RDI,
    Register::R10,
    Register::R11,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

fn preference(reg: Register) -> usize {
    match reg {
        Register::RCX => 12,
        Register::RSI => 11,
        Register::RDI => 10,
        Register::R10 => 9,
        Register::R11 => 8,
        Register::RAX => 7,
        Register::RDX => 6,
        Register::RBX => 5,
        Register::R12 => 4,
        Register::R13 => 3,
        Register::R14 => 2,
        Register::R15 => 1,
        _ => 0,
    }
}

pub const SCRATCH_1: Register = Register::R8;
pub const SCRATCH_2: Register = Register::R9;

#[derive(PartialEq, Eq)]
struct RegWithPreference(Register);

impl PartialOrd for RegWithPreference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.cmp(other).into()
    }
}

impl Ord for RegWithPreference {
    fn cmp(&self, other: &Self) -> Ordering {
        preference(self.0)
            .cmp(&preference(other.0))
            .then_with(|| self.0.cmp(&other.0))
    }
}
