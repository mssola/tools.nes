use std::fs::File;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Version for this program.
const VERSION: &str = "0.1.0";

// Arguments for this application. See `parse_arguments` on how it's filled.
#[derive(Default)]
struct Args {
    file: String,
    config: Option<String>,
    target: Option<String>,
    out: String,
}

// Print the help message and quit.
fn print_help() {
    println!("Bridge between 'nasm' and 'ca65'.\n");
    println!("usage: xa65 [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -C, --config <FILE>\tLinker configuration to be used, whether an identifier or a file path.");
    println!("  -o, --out <FILE>\tFile path where the output should be located after execution.");
    println!("  --target nes\t\tUsed for compatibility with 'ca65'.");
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
            "-C" | "--config" => match res.config {
                Some(_) => die("only specify the '-C/--config' flag once".to_string()),
                None => match args.next() {
                    Some(v) => res.config = Some(v),
                    None => {
                        die("you need to provide a value for the '-C/--config' flag".to_string())
                    }
                },
            },
            "-h" | "--help" => print_help(),
            "-o" | "--out" => {
                if res.out.is_empty() {
                    match args.next() {
                        Some(v) => res.out = v,
                        None => {
                            die("you need to provide a value for the '-o/--out' flag".to_string())
                        }
                    }
                } else {
                    die("only specify the '-o/--out' flag once".to_string());
                }
            }
            "--target" => match res.target {
                Some(_) => die("only specify the '--target' flag once".to_string()),
                None => match args.next() {
                    Some(v) => {
                        let real = v.to_lowercase();
                        if real != "nes" {
                            die("the '--target' flag only accepts 'nes' as a value".to_string());
                        }
                        res.target = Some(real)
                    }
                    None => die("you need to provide a value for the '--target' flag".to_string()),
                },
            },
            "-v" | "--version" => {
                println!("xa65 {}", VERSION);
                std::process::exit(0);
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
    if res.out.is_empty() {
        die("you need to specify an output file with '-o/--output'".to_string());
    }

    res
}

// Print the given `message` and exit(1).
fn die(message: String) {
    eprintln!("error: {}", message);
    std::process::exit(1);
}

// Find the binary by `name` in "PATH". Implementation taken from:
// https://stackoverflow.com/a/37499032.
fn find_binary(name: &str) -> Option<PathBuf> {
    std::env::var_os("PATH").and_then(|paths| {
        std::env::split_paths(&paths)
            .filter_map(|dir| {
                let full_path = dir.join(name);
                if full_path.is_file() {
                    Some(full_path)
                } else {
                    None
                }
            })
            .next()
    })
}

// Returns the path for the binaries for 'nasm' and 'cl65'.
fn get_binaries() -> Result<(PathBuf, PathBuf), String> {
    let nasm = match find_binary("nasm") {
        Some(nasm) => nasm,
        None => return Err("could not find 'nasm'".to_string()),
    };
    let cl65 = match find_binary("cl65") {
        Some(cl65) => cl65,
        None => return Err("could not find 'cl65'".to_string()),
    };

    Ok((nasm, cl65))
}

// Returns the path to the temporary directory that can be used for the run.
fn temporary_dir() -> PathBuf {
    let tmp = &std::env::temp_dir();
    let paths = std::fs::read_dir(tmp).unwrap();
    let name = format!("xa65-{}", paths.count());

    tmp.join(name)
}

// Attemps to generate an 'hexdump' with the given `bin` and taking the given
// `src` as an argument for 'hexdump'. The resulting dump will be saved in
// `dst`.
fn hexdump(bin: &PathBuf, src: &PathBuf, dst: &PathBuf) -> bool {
    let Ok(nasm) = Command::new(bin).arg("-C").arg(src).output() else {
        println!(
            "xa65 (warning): could not produce an hexdump of '{}'",
            src.display()
        );
        return false;
    };
    let Ok(mut nasm_file) = File::create(dst) else {
        println!(
            "xa65 (warning): could not produce an hexdump of '{}'",
            src.display()
        );
        return false;
    };
    if nasm_file.write_all(nasm.stdout.as_slice()).is_err() {
        println!(
            "xa65 (warning): could not produce an hexdump of '{}'",
            src.display()
        );
        return false;
    }
    true
}

// Attempt to generate 'hexdump' files for both binaries. If this is not
// possible, then it will print a warning and return early.
fn attempt_hexdump(dir: &Path) {
    let Some(bin) = find_binary("hexdump") else {
        println!(
            "xa65 (warning): could not find 'hexdump' in your PATH. \
                  A human-readable dump will not be generated"
        );
        return;
    };

    if hexdump(&bin, &dir.join("nasm.nes"), &dir.join("nasm.txt")) {
        hexdump(&bin, &dir.join("cl65.nes"), &dir.join("cl65.txt"));
    }
}

fn main() {
    // Make sure that the binaries are there.
    let (nasm, cl65) = match get_binaries() {
        Ok((nasm, cl65)) => (nasm, cl65),
        Err(e) => {
            die(e);
            return;
        }
    };

    // Parse arguments.
    let args = parse_arguments();

    // Generate a temporary directory in which both binary files will be placed
    // as an intermediate step.
    let dir = temporary_dir();
    if let Err(e) = std::fs::create_dir(&dir) {
        die(e.to_string());
        return;
    }

    // Run 'nasm' with the given arguments. Note that we don't care whether
    // 'nasm' itself errors out.
    if let Err(e) = Command::new(nasm)
        .arg(&args.file)
        .arg("-o")
        .arg(dir.join("nasm.nes"))
        .arg("-c")
        .arg(args.config.clone().unwrap_or("nrom65".to_string()))
        .status()
    {
        die(e.to_string());
        return;
    }

    // Run 'cl65' with the given arguments.
    let mut cl65_command = Command::new(cl65);
    cl65_command
        .arg("--target")
        .arg("nes")
        .arg(&args.file)
        .arg("-o")
        .arg(dir.join("cl65.nes"));
    if let Some(config) = &args.config {
        cl65_command.arg("-C").arg(config);
    }

    // Here, and in contrast with the 'nasm' execution, we do care about the
    // exit code of 'cl65'.
    match cl65_command.status() {
        Ok(cmd) => {
            if !cmd.success() {
                std::process::exit(1);
            }
        }
        Err(e) => {
            die(e.to_string());
            return;
        }
    }

    // Everything went fine, we should have both binaries available to be
    // compared. For 'diff' actually capture the output so it does not pollute
    // the shell.
    match Command::new("diff")
        .arg(dir.join("nasm.nes"))
        .arg(dir.join("cl65.nes"))
        .output()
    {
        Ok(diff) => {
            // If 'diff' failed, show it but don't error out.
            if !diff.status.success() {
                // Try to generate a human-readable diff.
                attempt_hexdump(&dir);

                println!(
                    "xa65 (error): 'nasm' and 'ca65' have a mismatch. Check the results at {}",
                    dir.display()
                );
            }
        }
        Err(e) => {
            die(e.to_string());
            return;
        }
    }

    // And just copy one of the binaries to where it was originally requested.
    // Note that the binary is the one from 'cl65' just in case 'diff' failed
    // (we take 'cl65' as the source of truth).
    if let Err(e) = std::fs::copy(dir.join("cl65.nes"), args.out) {
        die(format!("could not copy the resulting binary: {}", e));
    }
}
