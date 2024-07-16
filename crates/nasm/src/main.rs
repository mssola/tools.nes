use anyhow::Result;
use clap::Parser as ClapParser;
use std::fs::File;
use std::io::{self, Read, Write};
use xixanta::assembler::Assembler;
use xixanta::instruction::Fill;
use xixanta::mapping::Segment;

/// Assembler for the 6502 microprocessor that targets the NES.
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assemble the instructions given on this file. The standard input is used
    /// when this argument is not given.
    file: Option<String>,

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
    // TODO: provide a handier way for this.
    let mut assembler = Assembler::new(vec![Segment {
        name: String::from("CODE"),
        start: 0x1000,
        size: 10,
        fill_value: Some(Fill { value: 0x00 }),
    }]);

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

    // After the parse operation, just print the results.
    if args.disassemble {
        let instructions = assembler.disassemble(input)?;

        for instr in instructions {
            output.write_all(instr.to_human().as_bytes())?;
            output.write_all("\n".as_bytes())?;
        }
    } else {
        let instructions = assembler.assemble(input)?;

        for instr in instructions {
            let bs = instr.to_bytes();

            if instr.size() == 1 {
                output.write_all(&[bs[0]])?;
            } else if instr.size() == 2 {
                output.write_all(&[bs[0], bs[1]])?;
            } else {
                output.write_all(&[bs[0], bs[1], bs[2]])?;
            }
        }
    }

    Ok(())
}
