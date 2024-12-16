use anyhow::Result;
use clap::Parser as ClapParser;
use std::fs::File;
use std::io::{self, Read, Write};
use xixanta::assembler::Assembler;
use xixanta::mapping::{Mapping, EMPTY, NROM, NROM65};

/// Assembler for the 6502 microprocessor that targets the NES.
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assemble the instructions given on this file. The standard input is used
    /// when this argument is not given.
    file: Option<String>,

    /// Linker configuration to be used. Defaults to 'nrom'.
    #[arg(short = 'c', long)]
    config: Option<String>,

    /// Disassemble instead of assembling. Disabled by default.
    #[arg(short, long, default_value_t = false)]
    disassemble: bool,

    /// Place the output into the given <OUT> file. Ignored if the `stdout` flag
    /// is provided. Defaults to `out.nes` when assembling, and to `out.s` when
    /// disassembling.
    #[arg(short = 'o', long)]
    out: Option<String>,

    /// Spit the output into the standard output instead. This ignores any given
    /// `out` flag. Disabled by default.
    #[arg(long, default_value_t = false)]
    stdout: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Select the input stream.
    let input: Box<dyn Read> = match args.file {
        Some(file) => Box::new(File::open(file)?),
        None => Box::new(std::io::stdin()),
    };

    // Select the output stream.
    let mut output: Box<dyn Write> = if args.stdout {
        Box::new(io::stdout())
    } else {
        match args.out {
            Some(file) => Box::new(File::create(file)?),
            None => Box::new(File::create(if args.disassemble {
                "out.s"
            } else {
                "out.nes"
            })?),
        }
    };

    // Select the linker configuration.
    let mapping: Vec<Mapping> = match args.config {
        Some(c) => match c.to_lowercase().as_str() {
            "empty" => EMPTY.to_vec(),
            "nrom" => NROM.to_vec(),
            "nrom65" => NROM65.to_vec(),
            _ => {
                println!("Unnown linker configuration '{}'", c);
                std::process::exit(1);
            }
        },
        None => NROM.to_vec(),
    };

    // Initialize the assembler with the given linker configuration.
    let mut assembler = Assembler::new(mapping);

    // After the parse operation, just print the results.
    if args.disassemble {
        // let instructions = assembler.disassemble(input)?;

        // for instr in instructions {
        //     output.write_all(instr.to_human().as_bytes())?;
        //     output.write_all("\n".as_bytes())?;
        // }
    } else {
        match assembler.assemble(input) {
            Ok(bundles) => {
                for b in bundles {
                    for i in 0..b.size {
                        output.write_all(&[b.bytes[i as usize]])?;
                    }
                }
            }
            Err(errors) => {
                for err in errors {
                    println!("{}", err);
                }
                std::process::exit(1);
            }
        }
    }

    Ok(())
}
