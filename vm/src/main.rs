use vm::Processor;

use std::io::prelude::*;

fn main() {
    pretty_env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        eprintln!("Usage: vm <binary file>");
        return;
    }

    #[rustfmt::skip]
    let mut instrs = Vec::new();
    let mut file = std::fs::File::open(&args[1]).unwrap();
    file.read_to_end(&mut instrs).unwrap();

    Processor::new(instrs.into_boxed_slice()).run()
}
