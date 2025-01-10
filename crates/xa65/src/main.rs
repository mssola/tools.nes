use clap::Parser as ClapParser;
use rand::distributions::{Alphanumeric, DistString};
use std::path::PathBuf;
use std::process::Command;

/// Bridge between 'nasm' and 'ca65'.
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

    /// Place the output into the given <OUT> file.
    #[arg(short = 'o', long)]
    out: String,
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

fn main() {
    // Make sure that the binaries are there.
    let (nasm, cl65) = match get_binaries() {
        Ok((nasm, cl65)) => (nasm, cl65),
        Err(e) => {
            die(e);
            return;
        }
    };
    let args = Args::parse();

    // Generate a temporary directory in which both binary files will be placed
    // as an intermediate step.
    let random_string = &Alphanumeric.sample_string(&mut rand::thread_rng(), 16);
    let dir = std::env::temp_dir().join(random_string);
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
        cl65_command.arg("-c").arg(config);
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
