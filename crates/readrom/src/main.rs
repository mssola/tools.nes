use header::{Header, Kind};
use std::fs::File;
use std::io::{ErrorKind, Read};

/// Version for this program.
const VERSION: &str = "0.1.0";

#[derive(Default)]
struct Args {
    file: String,
    header: bool,
}

fn print_help() {
    println!("Display information about NES/Famicom ROM files.\n");
    println!("usage: readrom [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -H, --header\tJust print the ROM header and quit.");
    std::process::exit(0);
}

// Parse the arguments given to the program and returns an Args object with the
// given information.
fn parse_arguments() -> Args {
    let mut args = std::env::args();
    let mut res = Args::default();

    // Skip command name.
    args.next();

    for arg in args {
        match arg.as_str() {
            "-h" | "--help" => print_help(),
            "-H" | "--header" => {
                if res.header {
                    die("do not specify the '-H/--header' flag twice".to_string());
                }
                res.header = true;
            }
            "-v" | "--version" => {
                println!("readrom {VERSION}");
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
        die("you need to specify the file to be read".to_string());
    }

    res
}

fn print_header(header: &Header) {
    println!("Header:");

    match header.kind {
        Kind::INes => println!("  Kind:\t\tiNES"),
        Kind::Nes20 => println!("  Kind:\t\tNES 2.0"),
    }

    println!(
        "  PRG ROM size:\t{} bytes ({}KB)",
        header.prg_rom_size * 16 * 1024,
        header.prg_rom_size * 16
    );
    println!(
        "  CHR ROM size:\t{} bytes ({}KB)",
        header.chr_rom_size * 8 * 1024,
        header.chr_rom_size * 8
    );
    println!("  Mapper:\t{}", header.mapper);
}

fn print_vectors(addrs: &[u8]) {
    println!("Vectors:");
    println!(
        "  NMI:\t\t{:#04x}",
        u16::from_le_bytes([addrs[0], addrs[1]])
    );
    println!(
        "  Reset:\t{:#04x}",
        u16::from_le_bytes([addrs[2], addrs[3]])
    );
    println!(
        "  IRQ:\t\t{:#04x}",
        u16::from_le_bytes([addrs[4], addrs[5]])
    );
}

// Print the given `message` and exit(1).
fn die(message: String) {
    println!("error: {message}");
    std::process::exit(1);
}

fn main() {
    let args = parse_arguments();

    let Ok(mut input) = File::open(&args.file) else {
        die(format!("failed to open the given file '{}'", &args.file));
        return;
    };

    // Header.

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
            return;
        }
    };
    print_header(&header);

    if args.header {
        std::process::exit(0);
    }

    // PRG ROM.

    buf = vec![0u8; header.prg_rom_size * 16 * 1024];
    if let Err(e) = input.read_exact(&mut buf) {
        match e.kind() {
            ErrorKind::UnexpectedEof => die("could not read advertised PRG ROM space".to_string()),
            _ => die(e.to_string()),
        }
    }

    // Vectors.

    let vectors = &buf.as_slice()[buf.len() - 6..];
    print_vectors(vectors);
}
