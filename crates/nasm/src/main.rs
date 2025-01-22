use std::fs::File;
use std::io::{self, Write};
use std::path::Path;
use xixanta::assembler::assemble;
use xixanta::SourceInfo;

/// Version for this program.
const VERSION: &str = "0.1.0";

#[derive(Default)]
struct Args {
    file: String,
    config: Option<String>,
    out: Option<String>,
    werror: bool,
    stdout: bool,
}

// Print the help message and quit.
fn print_help() {
    println!("Assembler for the 6502 microprocessor that targets the NES/Famicom.\n");
    println!("usage: nasm [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -c, --config <FILE>\tLinker configuration to be used, whether an identifier or a file path.");
    println!("  -o, --out <FILE>\tFile path where the output should be located after execution.");
    println!("  --stdout\t\tPrint the output binary to the standard output.");
    println!("  -Werror\t\tWarnings should be treated as errors.");
    std::process::exit(0);
}

// Parse the arguments given to the program and returns an Args object with the
// given information.
fn parse_arguments() -> Args {
    let mut args = std::env::args();
    let mut res = Args::default();

    // Skip command name.
    args.next();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-c" | "--config" => match res.config {
                Some(_) => die("only specify the '-C/--config' flag once".to_string()),
                None => match args.next() {
                    Some(v) => res.config = Some(v),
                    None => {
                        die("you need to provide a value for the '-C/--config' flag".to_string())
                    }
                },
            },
            "-h" | "--help" => print_help(),
            "-o" | "--out" => match res.out {
                Some(_) => die("only specify the '-o/--out' flag once".to_string()),
                None => {
                    if res.stdout {
                        die("you cannot mix '-o/--out' and '--stdout'".to_string());
                    }
                    match args.next() {
                        Some(v) => res.out = Some(v),
                        None => {
                            die("you need to provide a value for the '-o/--out' flag".to_string())
                        }
                    }
                }
            },
            "--stdout" => match res.out {
                Some(_) => die("you cannot mix '-o/--out' and '--stdout'".to_string()),
                None => {
                    if res.stdout {
                        die("only specify the '--stdout' flag once".to_string());
                    }
                    res.stdout = true;
                }
            },
            "-v" | "--version" => {
                println!("nasm {}", VERSION);
                std::process::exit(0);
            }
            "-Werror" => {
                if res.werror {
                    die("only specify the '-Werror' flag once".to_string());
                }
                res.werror = true;
            }
            _ => {
                if arg.starts_with('-') {
                    die(format!("don't know how to handle the '{}' flag", arg));
                }
                if !res.file.is_empty() {
                    die("cannot have multiple source files".to_string());
                }
                res.file = arg;
            }
        }
    }

    if res.file.is_empty() {
        die("you need to specify a source file".to_string());
    }

    res
}

// Print the given `message` and exit(1).
fn die(message: String) {
    eprintln!("error: {}", message);
    std::process::exit(1);
}

fn main() {
    let args = parse_arguments();

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
        if args.werror {
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
