//! ALU
//!
//! The `alu()` function transforms `AluAction`s into `StackAction`s

use crate::StackAction;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum AluAction {
    Add,
    Not,
    Swap,
}

impl AluAction {
    // Should be u2
    pub fn to_bits(self) -> u8 {
        self as u8
    }

    pub fn from_bits(bits: u8) -> Self {
        use AluAction::*;
        match bits {
            0 => Add,
            1 => Not,
            2 => Swap,
            _ => panic!("bad bits"),
        }
    }
}

pub fn alu(action: AluAction, elems: (u8, u8)) -> StackAction {
    trace!("{:?}", action);
    use AluAction::*;
    match action {
        Add => StackAction::One(elems.0.wrapping_add(elems.1)),
        Not => StackAction::Two(elems.0, !elems.1),
        Swap => StackAction::Two(elems.1, elems.0),
    }
}
