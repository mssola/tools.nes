use clap::{arg, Arg, Command};
use header::{Header, Kind};
use std::fs::File;
use std::io::{ErrorKind, Read};

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
    println!("error: {}", message);
    std::process::exit(1);
}

fn main() {
    let args = Command::new("readrom")
        .version("0.1.0")
        .about("Display information about NES/Famicom ROM files.")
        .arg(Arg::new("FILE").required(true).help("ROM file to be read"))
        .arg(arg!(-H --header "Just print the ROM header and quit"))
        .get_matches();
    let file = match args.get_one::<String>("FILE") {
        Some(file) => file,
        None => {
            die("you have to provide a file".to_string());
            return;
        }
    };

    let Ok(mut input) = File::open(file) else {
        die(format!("failed to open the given file '{}'", &file));
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

    if *args.get_one::<bool>("header").unwrap() {
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
