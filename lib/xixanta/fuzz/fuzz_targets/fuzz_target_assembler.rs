#![no_main]

use libfuzzer_sys::fuzz_target;
use xixanta::assembler::assemble;

fuzz_target!(|data: &[u8]| {
    let _ = assemble(
        data,
        "empty",
        std::env::current_dir().unwrap().to_path_buf(),
    );
});
