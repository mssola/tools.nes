#![no_main]

use libfuzzer_sys::fuzz_target;
use xixanta::assembler::Assembler;
use xixanta::mapping::EMPTY;

fuzz_target!(|data: &[u8]| {
    let mut asm = Assembler::new(EMPTY.to_vec());
    let _ = asm.assemble(std::env::current_dir().unwrap().to_path_buf(), data);
});
