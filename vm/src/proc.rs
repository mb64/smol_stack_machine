//! Main processor -- just tying it all together

use crate::*;

pub struct Processor {
    pub imem: IMem,
    pub stack: Stack,
}

impl Processor {
    pub fn new(instrs: Box<[u8]>) -> Self {
        Self {
            imem: IMem::new(instrs),
            stack: Stack::new(),
        }
    }

    pub fn single_step(&mut self) {
        trace!("IP: 0x{:02}", self.imem.ip);
        let inst = self.imem.get();
        trace!("Instruction: 0x{:02x}\t0x{:02x}", inst.0, inst.1);
        let action = decoder::decode(inst, self.stack.peek());
        trace!("{:?}", action);
        self.imem.action(action.imem);
        self.stack.action(action.stack);
        send::send(action.send);
    }

    pub fn run(&mut self) -> ! {
        loop {
            self.single_step();
        }
    }
}
