use std::path::PathBuf;

use vnf::{Machine, MemoryPolicy};
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

fn run_break_mark_test(path: &String) -> Result<(), String> {
    let rom = PathBuf::from(path).join("out/stack.nes");
    let mut machine = Machine::from(&rom, 0x8000, MemoryPolicy::default())?;

    // Get out of <start>
    let _ = machine.next_iteration();

    // Assert that we are at the expected location.
    assert!(matches!(
        machine.current_instruction.identifier,
        InstructionIdentifier::Brk
    ));

    // Before vs after running 'brk'.
    assert_eq!(machine.status_register.break_mark, 0);
    assert!(!machine.status_register.brk);
    let _ = machine.next_iteration();
    assert_eq!(machine.status_register.break_mark, 0x42);
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
    assert_eq!(machine.status_register.break_mark, 0x42);
    assert!(machine.status_register.brk);

    // After running 'plp'.
    let _ = machine.next_iteration();
    assert_eq!(machine.status_register.break_mark, 0x00);
    assert!(!machine.status_register.brk);

    Ok(())
}

fn main() {
    let args = parse_arguments();
    let file = PathBuf::from(args.file.clone());

    if !file.is_dir() {
        die("you have to provide a path to the tests/ directory".to_string());
    }

    if let Err(e) = run_break_mark_test(&args.file) {
        die(e)
    }
}
