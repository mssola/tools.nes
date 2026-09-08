use std::path::{Path, PathBuf};

use vnf::{Joypad, Machine, MemoryPolicy};
use xixanta::opcodes::InstructionIdentifier;

#[derive(Default)]
struct Args {
    file: String,
}

fn print_help() {
    println!("End-to-end tests for the vnf library\n");
    println!("usage: vnf-tests [OPTIONS] <tests/ directory>\n");
    println!("Options:");
    println!("  -h, --help\t\t\tPrint this message and quit.");
    std::process::exit(0);
}

// Print the given `message` and exit(1).
fn die(message: String) -> ! {
    eprintln!("error: {message}");
    std::process::exit(1);
}

fn parse_arguments() -> Args {
    let mut args = std::env::args();
    let mut res = Args::default();

    // Skip command name.
    args.next();

    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => print_help(),
            _ => {
                if arg.starts_with('-') {
                    die(format!("don't know how to handle the '{arg}' flag"));
                }
                if !res.file.is_empty() {
                    die("cannot have multiple paths".to_string());
                }
                res.file = arg;
            }
        }
    }

    if res.file.is_empty() {
        die("you have to provide a path to the tests/ directory".to_string());
    }

    res
}

fn prepare_run(id: &str, rom_name: &str, path: &Path) -> Option<PathBuf> {
    let value = std::env::var("VNF_TEST")
        .unwrap_or_else(|_| "".to_string())
        .to_lowercase();

    if value.is_empty() || value == id {
        println!("[vnf tests] :: Running '{id}'");
        return Some(path.join(format!("out/{rom_name}")));
    }

    None
}

// Returns true if the 'VERBOSE' environment variable is either set to 'true'
// or '1', false otherwise.
fn verbose() -> bool {
    let value = std::env::var("VNF_TESTS_VERBOSE")
        .unwrap_or_else(|_| "0".to_string())
        .to_lowercase();

    value == "true" || value == "1"
}

fn run_break_mark_test(path: &Path) -> Result<(), String> {
    let mut machine = Machine::from(path, 0x8000, MemoryPolicy::default())?;
    machine.verbose = verbose();
    machine.halt_on_brk = false;

    // Get out of <start>
    let _ = machine.next_iteration();

    // Assert that we are at the expected location.
    assert!(matches!(
        machine.current_instruction.identifier,
        InstructionIdentifier::Brk
    ));

    // Before vs after running 'brk'.
    assert!(machine.status_register.break_mark.is_none());
    assert!(!machine.status_register.brk);
    let _ = machine.next_iteration();
    assert_eq!(machine.status_register.break_mark.unwrap(), 0x42);
    assert!(machine.status_register.brk);

    // Skip instructions we don't care about here.
    for _ in 0..11 {
        let _ = machine.next_iteration();
    }

    // Before running 'plp', everything is still as expected.
    assert!(matches!(
        machine.current_instruction.identifier,
        InstructionIdentifier::Plp
    ));
    assert_eq!(machine.status_register.break_mark.unwrap(), 0x42);
    assert!(machine.status_register.brk);

    // After running 'plp'.
    let _ = machine.next_iteration();
    assert!(machine.status_register.break_mark.is_none());
    assert!(!machine.status_register.brk);

    Ok(())
}

fn run_joypad_test(path: &Path) -> Result<(), String> {
    let mut machine = Machine::from(path, 0x8000, MemoryPolicy::default())?;

    // Manually change the PC to the reset function.
    let len = machine.prg_rom.len();
    let high = (machine.prg_rom[len - 1] as u16) << 8;
    let low = machine.prg_rom[len - 2] as u16;
    machine.pc = (high + low) as usize;

    // Due to the code of joypad.s, there is a repeating read algorithm. Hence,
    // we can to repeat each button combination.
    machine.push_inputs_to(
        0,
        &[
            // First read.
            (Joypad::BUTTON_DOWN | Joypad::BUTTON_B),
            (Joypad::BUTTON_DOWN | Joypad::BUTTON_B),
            // Second read.
            (Joypad::BUTTON_DOWN | Joypad::BUTTON_B),
            (Joypad::BUTTON_DOWN | Joypad::BUTTON_B),
        ],
    );
    machine.run_function_mode = true;
    machine.verbose = verbose();
    machine.until_address(0xFFFF)?;

    assert_eq!(machine.ram[0x00].value, 2);
    assert_eq!(machine.ram[0x01].value, 0);
    assert_eq!(machine.ram[0x02].value, 1);
    assert_eq!(machine.ram[0x03].value, 0);
    assert_eq!(machine.ram[0x04].value, 1);

    Ok(())
}

fn main() {
    let args = parse_arguments();
    let file = PathBuf::from(args.file.clone());

    if !file.is_dir() {
        die("you have to provide a path to the tests/ directory".to_string());
    }

    // And tests!

    if let Some(path) = prepare_run("break_mark", "stack.nes", &file)
        && let Err(e) = run_break_mark_test(&path)
    {
        die(e)
    }
    if let Some(path) = prepare_run("joypad", "joypad.nes", &file)
        && let Err(e) = run_joypad_test(&path)
    {
        die(e)
    }
}
