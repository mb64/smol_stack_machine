//! Instruction memory

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum IMemAction {
    Next(bool),
    Jump(u8),
}

#[derive(Debug)]
pub struct IMem {
    instrs: Box<[u8]>,
    pub ip: u8,
}

impl IMem {
    /// IP starts at 0
    pub fn new(instrs: Box<[u8]>) -> Self {
        Self { instrs, ip: 0 }
    }

    /// (current instr, next byte)
    pub fn get(&self) -> (u8, u8) {
        (
            self.instrs[self.ip as usize],
            self.instrs
                .get(self.ip as usize + 1)
                .map(|&x| x)
                .unwrap_or(0),
        )
    }

    pub fn action(&mut self, action: IMemAction) {
        use IMemAction::*;
        match action {
            Next(b) => {
                if b {
                    self.ip += 2
                } else {
                    self.ip += 1
                }
            }
            Jump(addr) => self.ip = addr,
        }
    }
}
