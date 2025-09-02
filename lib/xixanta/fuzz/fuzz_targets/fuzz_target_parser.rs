#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let mut parser = xixanta::parser::Parser::default();
    let _ = parser.parse(data, &xixanta::SourceInfo::default());
});
