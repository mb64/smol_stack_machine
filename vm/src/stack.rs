//! Stack
//!
//! The VM uses a `Vec<u8>`, but the hardware will actually do legit hardware things

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum StackAction {
    /// One(x) : a b -- x
    One(u8),
    /// Two(x, y) : a b -- x y
    Two(u8, u8),
    /// New(x) : a b -- a b x
    New(u8),
    /// Resurrect : -- a
    Resurrect,
    /// Nothing : --
    Nothing,
}

pub struct Stack {
    data: Vec<u8>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            data: vec![0, 0], // include two zeros to make it easier
        }
    }

    // Returns (second from top, top)
    // In hardware this will be cached
    pub fn peek(&self) -> (u8, u8) {
        (
            self.data[self.data.len() - 2],
            self.data[self.data.len() - 1],
        )
    }

    pub fn action(&mut self, action: StackAction) {
        use StackAction::*;
        match action {
            One(x) => {
                self.data.pop();
                self.data.pop();
                self.data.push(x);
            }
            Two(x, y) => {
                self.data.pop();
                self.data.pop();
                self.data.push(x);
                self.data.push(y);
            }
            New(x) => {
                self.data.push(x);
            }
            Resurrect => {
                // uhh this is super hacky anyways so
                unsafe { self.data.set_len(self.data.len() + 1) }
                // I fuckin hate Resurrect, wish there was a better way
            }
            Nothing => (),
        }
    }
}
