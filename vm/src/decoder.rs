//! Decoder
//!
//! Turns instructions into `StackAction`s and `IMemAction`s

use crate::alu;
use crate::{AluAction, IMemAction, SendAction, StackAction};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Action {
    pub stack: StackAction,
    pub imem: IMemAction,
    pub send: SendAction,
}

impl Action {
    // go to next instr
    fn stack(stack: StackAction) -> Self {
        Self {
            stack,
            imem: IMemAction::Next(false),
            send: None,
        }
    }
}

pub fn decode((instr, next_byte): (u8, u8), (x, y): (u8, u8)) -> Action {
    match instr {
        0x00 => Action {
            stack: StackAction::One(x), // Drop y
            imem: IMemAction::Next(false),
            send: Some(y),
        },
        0x01 => Action {
            stack: StackAction::One(x), // Drop y
            imem: IMemAction::Jump(y),
            send: None,
        },
        0x02 => Action {
            stack: StackAction::One(x), // Drop y
            imem: IMemAction::Next(y & 0x80 > 0),
            send: None,
        },
        0x03 => Action::stack(StackAction::One(x)),
        0x04..=0x06 => Action::stack(alu::alu(AluAction::from_bits(instr & 3), (x, y))),
        0x07 => Action::stack(StackAction::New(y)),
        0x08 => Action::stack(StackAction::Resurrect),
        0x09 => Action {
            stack: StackAction::New(next_byte),
            imem: IMemAction::Next(true),
            send: None,
        },
        _ => Action::stack(StackAction::Nothing),
    }
}
