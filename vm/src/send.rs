//! Output
//!
//! Pass a `SendAction` to `send()`
//!
//! In the VM, it outputs characters on stdout.
//! In the MVP FPGA implementation, it will output on an LED grid.

pub type SendAction = Option<u8>;

pub fn send(action: SendAction) {
    if let Some(c) = action {
        print!("{}", c as char);
    }
}
