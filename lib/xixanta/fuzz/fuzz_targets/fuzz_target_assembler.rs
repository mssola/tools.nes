#![no_main]

use libfuzzer_sys::fuzz_target;
use xixanta::assembler::assemble;

fuzz_target!(|data: &[u8]| {
    let _ = assemble(data, "empty", xixanta::SourceInfo::default());
});
