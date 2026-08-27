use header::Header;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, ErrorKind, Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use vnf::{Machine, MemoryPolicy};

/// Version for this program.
const VERSION: &str = "0.1.0";

#[derive(Default)]
struct Args {
    file: String,
    start: Option<u16>,
    assume_function: bool,
    nasm: Option<PathBuf>,
    dump_memory: bool,
    until_address: u16,
}

fn print_help() {
    println!("Run an NES/Famicom ROM to test its code under a set of conditions.\n");
    println!("usage: runrom [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -d, --dump-memory\t\tShow the memory that has changed after a run.");
    println!("  -f, --function\t\tRun the code by assuming it's a function.");
    println!("  -h, --help\t\t\tPrint this message and quit.");
    println!("  -n, --nasm-directory <PATH>\tPath to the .nasm/ directory.");
    println!("  -s, --start\t\t\tAddress from where to start (default: reset vector).");
    println!("  -v, --version\t\t\tPrint version information.");
    std::process::exit(0);
}

// Print the given `message` and exit(1).
fn die(message: String) -> ! {
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

// Fetch the address mapping from the .nasm/addresses.txt file. You need to pass
// the full 'path' to the .nasm/ directory for the project (i.e. the '-n/--nasm'
// option).
fn fetch_addresses(path: &Path) -> Result<HashMap<String, usize>, String> {
    let mut addresses: HashMap<String, usize> = HashMap::default();

    if let Ok(file) = File::open(path.join("addresses.txt")) {
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line.map_err(|e| e.to_string())?;
            let columns: Vec<&str> = line.split(',').map(|s| s.trim()).collect();
            if columns.len() != 3 {
                return Err("badly formatted address file".to_string());
            }

            let parsed_start = usize::from_str_radix(columns[1], 16)
                .map_err(|_| format!("invalid hex value: '{}'", columns[1]))?;
            addresses.insert(columns[0].to_string(), parsed_start);
        }
    }

    Ok(addresses)
}

// Parse the given 'val' as if it was an hexadecimal literal. If that fails,
// pick up whether a 'nasm' directory was provided (i.e. '-n/--nasm' option),
// and try to find a mapping on the 'addresses' map. If that map is empty, then
// it will be filled by parsing the "addresses.txt" file from the 'nasm'
// directory.
fn parse_hex_or_reference(
    val: String,
    nasm: &Option<PathBuf>,
    addresses: &mut HashMap<String, usize>,
) -> u16 {
    match parse_hex_argument(&val) {
        Ok(n) => n,
        Err(e) => match nasm {
            Some(nasm_path) => {
                if addresses.is_empty() {
                    *addresses = match fetch_addresses(nasm_path) {
                        Ok(addr) => addr,
                        Err(err) => die(err),
                    };
                }
                match addresses.get(&val) {
                    Some(v) => *v as u16,
                    None => die(format!("could not find '{val}'")),
                }
            }
            None => die(e),
        },
    }
}

fn parse_arguments() -> Args {
    let mut args = std::env::args();
    let mut res = Args::default();
    let mut start = None;
    let mut until_address = None;

    // Skip command name.
    args.next();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-h" | "--help" => print_help(),
            "-s" | "--start" => {
                if res.start.is_some() {
                    die("do not specify the '-s/--start' flag twice".to_string());
                }
                start = args.next();
                if start.is_none() {
                    die("you need to specify a value for the -s/--start flag!".to_string());
                }
            }
            "-d" | "--dump-memory" => {
                res.dump_memory = true;
            }
            "-f" | "--function" => {
                res.assume_function = true;
            }
            "-n" | "--nasm" => match args.next() {
                Some(a) => {
                    let pb = PathBuf::from(a.clone());
                    if !pb.exists() {
                        die(format!("directory '{a}' does not exist"));
                    }
                    if !pb.is_dir() {
                        die(format!("path '{a}' does not point to a directory"));
                    }
                    res.nasm = Some(pb);
                }
                None => die("you need to specify a file for the '-n/--nasm' flag".to_string()),
            },
            "--until-address" => {
                until_address = args.next();
                if until_address.is_none() {
                    die("you need to specify a value for the --until-address flag!".to_string());
                }
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

    // Further handle options which can be either an hexadecimal value or an
    // address reference.

    let mut addresses = HashMap::new();

    if let Some(val) = start {
        res.start = Some(parse_hex_or_reference(val, &res.nasm, &mut addresses));
    }
    res.until_address = match until_address {
        Some(val) => parse_hex_or_reference(val, &res.nasm, &mut addresses),
        None => 0xFFFF,
    };

    // And finally, check that a ROM file was actually provided.

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
        Err(e) => die(e.to_string()),
    };

    // 2. With a known PRG ROM size, fetch the two bytes pertaining to the reset
    // vector.

    // The two bytes of the reset address are located as follows:
    //   1. Skip the ROM header, guaranteed to be exactly 0x10 bytes long.
    //   2. Go to the end of PRG ROM.
    //   3. -6: NMI address; -4: reset address; -2: IRQ address.
    let offset: u64 = (0x10 + (header.prg_rom_size * 16 * 1024) - 4)
        .try_into()
        .unwrap();

    if input.seek(SeekFrom::Start(offset)).is_err() {
        die("cannot peek into the ROM's reset address".to_string());
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

fn run(
    file: &String,
    start: u16,
    end: u16,
    assume_function: bool,
    dump_memory: bool,
) -> Result<(), String> {
    let mut machine = Machine::from(file, start, MemoryPolicy::default())?;

    machine.verbose = true;
    machine.run_function_mode = assume_function;

    machine.until_address(end)?;

    if dump_memory {
        let mut title = false;

        for (idx, cell) in machine.ram.iter().enumerate() {
            if cell.reads > 0 || cell.writes > 0 {
                if !title {
                    println!("\n== Memory dump ==\n");
                    title = true;
                }

                println!(
                    "[${:X}] = ${:02X} [reads={}, writes={}]",
                    idx, cell.value, cell.reads, cell.writes
                );
            }
        }
    }

    Ok(())
}

fn main() {
    let args = parse_arguments();
    let start = match args.start {
        Some(s) => s,
        None => start_from_reset_vector(&args.file),
    };

    match run(
        &args.file,
        start,
        args.until_address,
        args.assume_function,
        args.dump_memory,
    ) {
        Ok(m) => m,
        Err(e) => {
            die(e);
        }
    }
}
