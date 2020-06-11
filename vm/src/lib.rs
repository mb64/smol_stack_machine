//! Smol VM

#[macro_use]
extern crate log;

pub mod alu;
pub mod decoder;
pub mod imem;
pub mod proc;
pub mod send;
pub mod stack;

pub use alu::AluAction;
pub use decoder::Action;
pub use imem::{IMem, IMemAction};
pub use proc::Processor;
pub use send::SendAction;
pub use stack::{Stack, StackAction};
