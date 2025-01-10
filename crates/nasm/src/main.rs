use clap::Parser as ClapParser;
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;
use xixanta::assembler::assemble;
use xixanta::SourceInfo;

/// Assembler for the 6502 microprocessor that targets the NES/Famicom.
#[derive(ClapParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Assemble the instructions given on this file.
    file: String,

    /// Linker configuration to be used. This configuration can be an identifier
    /// for the configurations already baked in into this application, or it can
    /// be a file path to a configuration of your choosing. See the
    /// documentation for more information on this format. Defaults to 'nrom'.
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

// Print the given `message` and exit(1).
fn die(message: String) {
    eprintln!("error: {}", message);
    std::process::exit(1);
}

fn main() {
    let args = Args::parse();

    // Select the input stream and build the source object.
    let path = Path::new(&args.file);
    let Ok(input) = File::open(path) else {
        die(format!("failed to open the given file '{}'", &args.file));
        return;
    };
    let source = match path.parent() {
        Some(parent) => SourceInfo {
            directory: parent.to_path_buf(),
            name: path.file_name().unwrap().to_str().unwrap().to_string(),
        },
        None => {
            die("failed to find directory for the given file".to_string());
            return;
        }
    };

    // Select the output stream.
    let (mut output, output_name): (Box<dyn Write>, &str) = if args.stdout {
        (Box::new(io::stdout()), "<stdout>")
    } else {
        let name = args.out.unwrap_or(String::from("out.nes"));
        match File::create(&name) {
            Ok(f) => (Box::new(f), args.file.as_str()),
            Err(_) => {
                die(format!("could not create file '{}'", name));
                return;
            }
        }
    };

    // Check if warnings have to be treated as errors.
    let warn_as_errors = match args.w {
        Some(value) => {
            if value.to_lowercase() != "error" {
                die("the '-W' flag can only be used as '-Werror'".to_string());
                return;
            } else {
                true
            }
        }
        None => false,
    };

    // And assemble.
    let mut error_count = 0;
    let res = assemble(
        input,
        args.config.unwrap_or("nrom".to_string()).as_str(),
        source,
    );

    // Print warnings and errors first, while also computing the amount of them
    // that exists.
    for warning in res.warnings {
        if warn_as_errors {
            eprintln!("error: {}", warning);
            error_count += 1;
        } else {
            eprintln!("warning: {}", warning);
        }
    }
    for error in res.errors {
        eprintln!("error: {}", error);
        error_count += 1;
    }

    // If everything was right, just deliver the bundles.
    if error_count == 0 {
        for b in res.bundles {
            for i in 0..b.size {
                if let Err(e) = output.write_all(&[b.bytes[i as usize]]) {
                    eprintln!("error: could not write result in '{}': {}", output_name, e);
                    std::process::exit(1);
                }
            }
        }
    }

    std::process::exit(error_count);
}
