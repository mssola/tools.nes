use anyhow::{bail, Context, Result};
use clap::Parser as ClapParser;
use std::fs::File;
use std::io::{self, Read, Write};
use std::path::Path;
use xixanta::assembler::Assembler;
use xixanta::mapping::{Mapping, EMPTY, NROM, NROM65};

/// Assembler for the 6502 microprocessor that targets the NES/Famicom.
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assemble the instructions given on this file. The standard input is used
    /// when this argument is not given.
    file: Option<String>,

    /// Linker configuration to be used. Defaults to 'nrom'.
    #[arg(short = 'c', long)]
    config: Option<String>,

    /// Place the output into the given <OUT> file. Ignored if the `stdout` flag
    /// is provided. Defaults to `out.nes`.
    #[arg(short = 'o', long)]
    out: Option<String>,

    /// Treat warnings as errors.
    #[arg(short = 'W', value_name = "Error")]
    w: Option<String>,

    /// Spit the output into the standard output instead. This ignores any given
    /// `out` flag. Disabled by default.
    #[arg(long, default_value_t = false)]
    stdout: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Select the input stream and the current working directory.
    let input: Box<dyn Read>;
    let working_directory = match &args.file {
        Some(file) => {
            let path = Path::new(file);
            if !path.is_file() {
                bail!("Input file must be a valid file");
            }
            input = Box::new(File::open(file)?);

            path.parent()
                .with_context(|| String::from("Failed to find directory for given file"))?
        }
        None => {
            input = Box::new(std::io::stdin());
            &std::env::current_dir()
                .with_context(|| String::from("Could not fetch current directory"))?
        }
    };

    // Select the output stream.
    let mut output: Box<dyn Write> = if args.stdout {
        Box::new(io::stdout())
    } else {
        Box::new(File::create(args.out.unwrap_or(String::from("out.nes")))?)
    };

    // Check if warnings have to be treated as errors.
    let warn_as_errors = match args.w {
        Some(value) => {
            if value.to_lowercase() != "error" {
                bail!("The '-W' flag can only be used as '-Werror'");
            } else {
                true
            }
        }
        None => false,
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

    // And assemble.
    let mut error_count = 0;
    let mut assembler = Assembler::new(mapping);
    match assembler.assemble(working_directory.to_path_buf(), input) {
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
                error_count += 1;
            }
        }
    }
    for warning in assembler.warnings() {
        println!("Warning: {}", warning);
        if warn_as_errors {
            error_count += 1;
        }
    }

    std::process::exit(error_count);
}
