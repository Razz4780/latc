use crate::middlend::{BlockId, Hir, HirFunction, HirLoc, PhiOption, VRegId, Value};
use std::{
    cmp,
    collections::{HashMap, HashSet},
    fmt::{self, Debug, Formatter},
};

/// A lifetime interval for a virtual register.
#[derive(PartialEq, Eq)]
pub struct Interval {
    begin: HirLoc,
    end: HirLoc,
    vreg: VRegId,
}

impl Debug for Interval {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: [{:?},{:?}]", self.vreg, self.begin, self.end)
    }
}

impl Interval {
    fn new(vreg: VRegId, begin: HirLoc, end: HirLoc) -> Self {
        Self { begin, end, vreg }
    }

    pub fn begin(&self) -> &HirLoc {
        &self.begin
    }

    pub fn end(&self) -> &HirLoc {
        &self.end
    }

    pub fn vreg(&self) -> VRegId {
        self.vreg
    }

    fn set_live(&mut self, at: HirLoc) -> &mut Self {
        self.begin = cmp::min(self.begin, at);
        self.end = cmp::max(self.end, at);
        self
    }

    fn set_begin(&mut self, at: HirLoc) -> &mut Self {
        self.begin = at;
        self
    }
}

/// A helper struct for generating lifetime [`Interval`]s inside a [`HirFunction`].
pub struct IntervalBuilder<'a> {
    fun: &'a HirFunction<'a>,
    livein: HashMap<BlockId, HashSet<VRegId>>,
    intervals: HashMap<VRegId, Interval>,
}

impl<'a> IntervalBuilder<'a> {
    /// Creates a new instance of this struct for the given [`HirFunction`].
    pub fn new(fun: &'a HirFunction<'a>) -> Self {
        Self {
            fun,
            livein: Default::default(),
            intervals: Default::default(),
        }
    }

    fn process_block(&mut self, id: BlockId) {
        let mut live: HashSet<VRegId> = Default::default();
        let block = self.fun.block(id);

        let block_first_loc = HirLoc {
            block: id,
            hir_idx: 0,
        };

        let block_last_loc = HirLoc {
            block: id,
            hir_idx: block.len() - 1,
        };

        for child in self.fun.children_of(id) {
            if let Some(l) = self.livein.get(child) {
                live.extend(l);
            }

            self.fun
                .block(*child)
                .iter()
                .filter_map(|hir| match hir {
                    Hir::Phi { options, .. } => Some(options),
                    _ => None,
                })
                .flatten()
                .filter_map(|opt| match opt {
                    PhiOption {
                        from,
                        val: Value::VReg(vreg),
                    } if *from == id => Some(vreg.id),
                    _ => None,
                })
                .for_each(|vreg| {
                    live.insert(vreg);
                });

            self.fun
                .block(*child)
                .iter()
                .filter_map(|hir| match hir {
                    Hir::Phi { dst, options } => {
                        Some([(dst, options[0].from), (dst, options[1].from)])
                    }
                    _ => None,
                })
                .flatten()
                .filter_map(|(dst, from)| if from == id { Some(dst) } else { None })
                .for_each(|dst| {
                    self.intervals
                        .entry(dst.id)
                        .or_insert_with(|| Interval::new(dst.id, block_last_loc, block_last_loc))
                        .set_live(block_last_loc);
                });
        }

        for vreg in live.iter().copied() {
            self.intervals
                .entry(vreg)
                .or_insert_with(|| Interval::new(vreg, block_first_loc, block_last_loc))
                .set_live(block_first_loc)
                .set_live(block_last_loc);
        }

        for hir_idx in (0..block.len()).rev() {
            let current_loc = HirLoc { block: id, hir_idx };
            let hir = &block[hir_idx];

            if let Hir::Phi { dst, .. } = hir {
                live.remove(&dst.id);
            } else {
                hir.inspect_used(|vreg| {
                    self.intervals
                        .entry(vreg.id)
                        .or_insert_with(|| Interval::new(vreg.id, block_first_loc, current_loc))
                        .set_live(block_first_loc)
                        .set_live(current_loc);
                    live.insert(vreg.id);
                });

                if let Some(vreg) = hir.get_defined() {
                    self.intervals
                        .entry(vreg.id)
                        .or_insert_with(|| Interval::new(vreg.id, current_loc, current_loc))
                        .set_begin(current_loc);
                    live.remove(&vreg.id);
                }
            }
        }

        let last_body_block = self
            .fun
            .parents_of(id)
            .iter()
            .copied()
            .find(|parent| *parent > id);
        if let Some(last_body_block) = last_body_block {
            let last_loop_loc = HirLoc {
                block: last_body_block,
                hir_idx: self.fun.block(last_body_block).len() - 1,
            };

            for vreg in live.iter().copied() {
                self.intervals
                    .entry(vreg)
                    .or_insert_with(|| Interval::new(vreg, block_first_loc, last_loop_loc))
                    .set_live(block_first_loc)
                    .set_live(last_loop_loc);
            }
        }

        self.livein.insert(id, live);
    }

    /// Generates lifetime [`Interval`]s.
    pub fn run(mut self) -> Vec<Interval> {
        for block_id in (0..self.fun.blocks().len()).rev().map(BlockId) {
            self.process_block(block_id);
        }

        self.intervals.into_values().collect()
    }
}
