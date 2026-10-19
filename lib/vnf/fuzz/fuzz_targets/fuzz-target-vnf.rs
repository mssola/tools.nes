#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let policy = vnf::MemoryPolicy::default();
    let Ok(s) = std::str::from_utf8(data) else {
        return;
    };
    let path = std::path::Path::new(s);
    let _ = vnf::Machine::from(path, 0x8000, policy);
});
