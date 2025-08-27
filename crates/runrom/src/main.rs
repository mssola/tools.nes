use header::Header;
use std::fs::File;
use std::io::{ErrorKind, Read, Seek, SeekFrom};
use vnf::{Machine, MemoryInitialValue, MemoryPolicy};

/// Version for this program.
const VERSION: &str = "0.1.0";

#[derive(Default)]
struct Args {
    file: String,
    start: Option<u16>,
    assume_function: bool,
}

fn print_help() {
    println!("Run an NES/Famicom ROM for testing purposes.\n");
    println!("usage: runrom [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -f, --function\tRun the code by assuming it's a function.");
    println!("  -h, --help\t\tPrint this message and quit.");
    println!("  -s, --start\t\tAddress from where to start (default: reset vector).");
    println!("  -v, --version\t\tPrint version information.");
    std::process::exit(0);
}

// Print the given `message` and exit(1).
fn die(message: String) {
    eprintln!("error: {message}");
    std::process::exit(1);
}

fn parse_hex_digit(c: char) -> Result<u16, String> {
    match c.to_digit(16) {
        Some(val) => Ok(val as u16),
        None => Err("cannot convert digit to hexadecimal".to_string()),
    }
}

fn parse_hex_argument(given: &str) -> Result<u16, String> {
    // Skip a leading '$' character.
    let arg = if given.starts_with('$') {
        given.get(1..).unwrap_or("")
    } else {
        given
    };
    let mut chars = arg.chars();

    match arg.len() {
        0 => Err("you need to provide an address".to_string()),
        1 => Ok(parse_hex_digit(chars.next().unwrap())?),
        2 => Ok((parse_hex_digit(chars.next().unwrap())? << 4)
            + (parse_hex_digit(chars.next().unwrap())?)),
        3 => Ok((parse_hex_digit(chars.next().unwrap())? << 8)
            + (parse_hex_digit(chars.next().unwrap())? << 4)
            + (parse_hex_digit(chars.next().unwrap())?)),
        4 => Ok((parse_hex_digit(chars.next().unwrap())? << 12)
            + (parse_hex_digit(chars.next().unwrap())? << 8)
            + (parse_hex_digit(chars.next().unwrap())? << 4)
            + (parse_hex_digit(chars.next().unwrap())?)),
        _ => Err("hex literal is too big".to_string()),
    }
}

fn parse_arguments() -> Args {
    let mut args = std::env::args();
    let mut res = Args::default();

    // Skip command name.
    args.next();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "--help" => print_help(),
            "-s" | "--start" => {
                if res.start.is_some() {
                    die("do not specify the '-s/--start' flag twice".to_string());
                }
                let Some(val) = args.next() else {
                    die("you need to specify a value for the -s/--start flag!".to_string());
                    return res;
                };
                match parse_hex_argument(&val) {
                    Ok(n) => res.start = Some(n),
                    Err(e) => die(e),
                }
            }
            "-f" | "--function" => {
                res.assume_function = true;
            }
            "-v" | "--version" => {
                println!("runrom {VERSION}");
                std::process::exit(0);
            }
            _ => {
                if arg.starts_with('-') {
                    die(format!("don't know how to handle the '{arg}' flag"));
                }
                if !res.file.is_empty() {
                    die("cannot have multiple source files".to_string());
                }
                res.file = arg;
            }
        }
    }

    if res.file.is_empty() {
        die("you need to specify the file to be run".to_string());
    }

    res
}

// Given a ROM file identified by the `file` parameter, fetch the 16-bit address
// as pointed out by the reset vector.
fn start_from_reset_vector(file: &String) -> u16 {
    // 1. Read the ROM header so we fetch the size of PRG ROM.

    let Ok(mut input) = File::open(file) else {
        die(format!("failed to open the given file '{file}'"));
        return 0;
    };

    let mut buf = vec![0u8; 0x10];
    if let Err(e) = input.read_exact(&mut buf) {
        match e.kind() {
            ErrorKind::UnexpectedEof => die("malformed ROM file".to_string()),
            _ => die(e.to_string()),
        }
    }

    let header = match Header::try_from(buf.as_slice()) {
        Ok(h) => h,
        Err(e) => {
            die(e.to_string());
            return 0;
        }
    };

    // 2. With a known PRG ROM size, fetch the two bytes pertaining to the reset
    // vector.

    // The two bytes of the reset address are located as follows:
    //   1. Skip the ROM header, guaranteed to be exactly 0x10 bytes long.
    //   2. Go to the end of PRG ROM.
    //   3. -6: NMI addres; -4: reset addres; -2: IRQ address.
    let offset: u64 = (0x10 + (header.prg_rom_size * 16 * 1024) - 4)
        .try_into()
        .unwrap();

    if input.seek(SeekFrom::Start(offset)).is_err() {
        die("cannot peek into the ROM's reset address".to_string());
        return 0;
    };
    let mut buf = [0u8; 0x02];
    if let Err(e) = input.read_exact(&mut buf) {
        match e.kind() {
            ErrorKind::UnexpectedEof => die("malformed ROM file".to_string()),
            _ => die(e.to_string()),
        }
    }

    ((buf[1] as u16) << 8) + buf[0] as u16
}

fn run(file: &String, start: u16, assume_function: bool) -> Result<(), String> {
    let mut machine = Machine::from(
        file,
        start,
        MemoryPolicy {
            initial_value: MemoryInitialValue::Fixed(0),
            allowed_reads: vec![(0..0x800)],
            allowed_writes: vec![(0..0x800)],
            minimum_stack_value: 0,
        },
    )?;
    machine.verbose = true;

    if assume_function {
        machine.run_function()
    } else {
        machine.until_address(0x9500)
    }
}

fn main() {
    let args = parse_arguments();
    let start = match args.start {
        Some(s) => s,
        None => start_from_reset_vector(&args.file),
    };

    match run(&args.file, start, args.assume_function) {
        Ok(m) => m,
        Err(e) => {
            die(e);
            return;
        }
    };
}
