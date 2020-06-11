use vm::Processor;

fn main() {
    pretty_env_logger::init();

    #[rustfmt::skip]
    let instrs = vec![
        0x09, b'H', // imm 'H'
        0x00,       // send
        0x09, b'i', // imm 'i'
        0x00,       // send
        0x09, b'\n', // imm '\n'
        0x00,       // send
    ]
    .into_boxed_slice();

    Processor::new(instrs).run()
}
