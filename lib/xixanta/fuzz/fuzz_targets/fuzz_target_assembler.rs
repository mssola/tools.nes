#![no_main]

use libfuzzer_sys::fuzz_target;
use xixanta::assembler::Assembler;
use xixanta::mapping::get_mapping_configuration;

fuzz_target!(|data: &[u8]| {
    let config = get_mapping_configuration("empty").unwrap();
    let mut asm = Assembler::new(config);
    let _ = asm.assemble(std::env::current_dir().unwrap().to_path_buf(), data);
});
