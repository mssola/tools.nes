use anyhow::{bail, Result};
use clap::Parser as ClapParser;
use header::{Header, Kind};
use std::fs::File;
use std::io::{ErrorKind, Read};

/// Display information about NES/Famicom ROM files.
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// NES/Famicom ROM file from which to display information.
    file: String,
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

fn main() -> Result<()> {
    let args = Args::parse();
    let mut input = File::open(args.file)?;

    let mut buf = vec![0u8; 0x10];
    if let Err(e) = input.read_exact(&mut buf) {
        match e.kind() {
            ErrorKind::UnexpectedEof => bail!("malformed ROM file."),
            _ => return Err(e.into()),
        }
    }

    let header = match Header::try_from(buf.as_slice()) {
        Ok(h) => h,
        Err(e) => bail!("{}.", e),
    };
    print_header(&header);

    Ok(())
}
