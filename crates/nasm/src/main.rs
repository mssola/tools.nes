use header::Header;
use std::fs::{create_dir, File};
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use xixanta::assembler::{assemble, MemoryResult};
use xixanta::mapping::Mapping;
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
    stats: bool,
    defines: Vec<(String, u8)>,
    asan: bool,
    info: bool,
}

// Print the help message and quit.
fn print_help() {
    println!("Assembler for the 6502 microprocessor that targets the NES/Famicom.\n");
    println!("usage: nasm [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -a, --asan\t\tEnable the Address Sanitizer.");
    println!("  -c, --config <FILE>\tLinker configuration to be used, whether an identifier or a file path.");
    println!("  -D <NAME>(=VALUE)\tDefine an 8-bit variable on the global scope (default: 1)");
    println!("  -h, --help\t\tPrint this message.");
    println!("  -o, --out <FILE>\tFile path where the output should be located after execution.");
    println!("  -s, --stats\t\tPrint the statistics on the final layout of segments and memory.");
    println!("  --stdout\t\tPrint the output binary to the standard output.");
    println!("  -v, --version\t\tPrint the version of this program.");
    println!(
        "  -w, --write-info\tWrite debug/analysis information into a special '.nasm' directory."
    );
    println!("  -Werror\t\tWarnings should be treated as errors.");
    std::process::exit(0);
}

// Parse a value from the '-D' flag which is expected to be 'NAME(=VALUE)'.
fn parse_define(arg: &str) -> (String, u8) {
    let mut key_value = arg.split('=');

    let Some(name) = key_value.next() else {
        die(format!("bad format for define '{arg}'"));
        return (String::default(), 0);
    };

    if name
        .chars()
        .any(|c| !c.is_ascii_alphanumeric() && c != '_' && c != '@' && c != '.')
    {
        die(format!(
            "trying to define '{arg}' which has invalid characters"
        ));
    }

    let value = match key_value.next().unwrap_or("1").parse::<u8>() {
        Ok(integer) => integer,
        Err(_) => {
            die(format!(
                "value for define '{arg}' must be a valid 8-bit integer"
            ));
            return (String::default(), 0);
        }
    };

    (name.to_string(), value)
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
            "a" | "--asan" => res.asan = true,
            "-c" | "--config" => match res.config {
                Some(_) => die("only specify the '-C/--config' flag once".to_string()),
                None => match args.next() {
                    Some(v) => res.config = Some(v),
                    None => {
                        die("you need to provide a value for the '-C/--config' flag".to_string())
                    }
                },
            },
            "-D" => match args.next() {
                Some(a) => res.defines.push(parse_define(&a)),
                None => die("you need to provide a value for the '-D' flag".to_string()),
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
            "-s" | "--stats" => res.stats = true,
            "-w" | "--write-info" => res.info = true,
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
                println!("nasm {VERSION}");
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
        die("you need to specify a source file".to_string());
    }

    res
}

// Print the given `message` and exit(1).
fn die(message: String) {
    eprintln!("error: {message}");
    std::process::exit(1);
}

// Given a `source` object, find the first parent directory (including
// `source.directory`) which has a `.git` subdirectory. If none could be found,
// then `source.directory` is returned.
fn find_git_directory(source: &SourceInfo) -> PathBuf {
    for dir in source.directory.ancestors() {
        if dir.join(".git").exists() {
            return dir.to_path_buf();
        }
    }
    source.directory.clone()
}

// Get the path to the closest `.nasm` special directory and create it if it's
// not available on the expected base directory.
fn get_directory_from_source(source: &SourceInfo) -> PathBuf {
    let dir = find_git_directory(source).join(".nasm");

    if !dir.exists() {
        if let Err(e) = create_dir(&dir) {
            die(e.to_string());
        }
    }
    dir
}

// Save the `memory` object into the `<source>/.nasm/memory.txt` file.
fn save_memory_stats(source: &SourceInfo, memory: &mut MemoryResult, has_working_ram: bool) {
    let Ok(mut file) = File::create(get_directory_from_source(source).join("memory.txt")) else {
        return die("could not write memory.txt file".to_string());
    };

    let ranges = &mut memory.memory_ranges;
    ranges.sort_by(|a, b| a.range.start.cmp(&b.range.start));

    for mr in ranges {
        if mr.range.start + 1 == mr.range.end {
            if let Err(e) = writeln!(file, "{}: {}", mr.range_to_human(), mr.name) {
                return die(format!("could not write memory.txt file: {e}"));
            }
        } else if let Err(e) = writeln!(file, "{}: {}", mr.range_to_human(), mr.name) {
            return die(format!("could not write memory.txt file: {e}"));
        }
    }

    if let Err(e) = writeln!(file, "\n--- Summary (in bytes) ---") {
        return die(format!("could not write memory.txt file: {e}"));
    }
    print_memory_summary(Box::new(file), memory, has_working_ram);
}

// Print a memory summary message as given in `memory` to the `output`
// stream. The Working RAM will only be reported if `has_working_ram` is set to
// true.
fn print_memory_summary(mut output: Box<dyn Write>, memory: &MemoryResult, has_working_ram: bool) {
    let perc = (memory.total_internal_ram * 100) as f64 / 2048.0;

    if let Err(e) = writeln!(
        output,
        "- Internal RAM: {}/2048 ({:.2}%)",
        memory.total_internal_ram, perc
    ) {
        return die(format!("could not write memory summary: {e}"));
    }

    if has_working_ram {
        let perc = (memory.total_working_ram * 100) as f64 / 8192.0;
        if let Err(e) = writeln!(
            output,
            "- Working RAM: {}/8192 ({:.2}%)",
            memory.total_working_ram, perc
        ) {
            die(format!("could not write memory summary: {e}"))
        }
    }
}

// Print to the `output` stream the statistics that can be gathered from the
// given `mappings`.
fn print_segments_stats(mut output: Box<dyn Write>, mappings: &[Mapping]) {
    for mapping in mappings {
        let perc = (mapping.offset * 100) as f64 / mapping.size as f64;

        if perc.fract().abs() < f64::EPSILON {
            if let Err(e) = writeln!(
                output,
                "- {}: {}/{} ({:.0}%)",
                mapping.name, mapping.offset, mapping.size, perc
            ) {
                return die(format!("could not write segments summary: {e}"));
            }
        } else if let Err(e) = writeln!(
            output,
            "- {}: {}/{} ({:.2}%)",
            mapping.name, mapping.offset, mapping.size, perc
        ) {
            return die(format!("could not write segments summary: {e}"));
        }
    }
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
                die(format!("could not create file '{name}'"));
                return;
            }
        }
    };

    // And assemble.
    let mut error_count = 0;
    let res = assemble(
        input,
        args.config.unwrap_or("nrom".to_string()).as_str(),
        &args.defines,
        &source,
        args.asan,
    );

    // Print warnings and errors first, while also computing the amount of them
    // that exists.
    for warning in res.warnings {
        if args.werror {
            eprintln!("error: {warning}");
            error_count += 1;
        } else {
            eprintln!("warning: {warning}");
        }
    }
    for error in res.errors {
        eprintln!("error: {error}");
        error_count += 1;
    }

    let mut has_working_ram = false;

    // Deliver the bundles unless the header is borked.
    if error_count == 0 {
        // Fetch the header first.
        let mut temptative_header = vec![];
        for b in &res.bundles {
            for i in 0..b.size {
                temptative_header.push(b.bytes[i as usize]);
            }
            if temptative_header.len() >= 0x10 {
                break;
            }
        }

        // Validate the resulting header.
        match Header::try_from(temptative_header.as_slice()) {
            Ok(header) => {
                if res.accessing_working_ram && !header.has_persistent_memory {
                    die(
                        "requires Working RAM but the ROM header does not advertise it".to_string(),
                    );
                }
                has_working_ram = header.has_persistent_memory;
            }
            Err(e) => {
                die(format!(
                    "output would produce a malformed NES/Famicom ROM: {e}"
                ));
            }
        };

        // And now deliver the bundles.
        for b in res.bundles {
            for i in 0..b.size {
                if let Err(e) = output.write_all(&[b.bytes[i as usize]]) {
                    eprintln!("error: could not write result in '{output_name}': {e}");
                    std::process::exit(1);
                }
            }
        }

        // Print segment statistics.
        if args.stats {
            if args.info {
                let Ok(file) =
                    File::create(get_directory_from_source(&source).join("segments.txt"))
                else {
                    return die("could not write segments.txt file".to_string());
                };
                print_segments_stats(Box::new(file), &res.mappings);
            }

            println!("== Statistics ==\n");
            println!("=> Amount of space on each segment (in bytes):\n");

            print_segments_stats(Box::new(io::stdout()), &res.mappings);
        }

        // Print memory statistics.
        if args.asan {
            let mut memory = res.memory;
            if args.info {
                save_memory_stats(&source, &mut memory, has_working_ram);
            }
            if args.stats {
                println!("\n=> Amount of memory used (in bytes):\n");
                print_memory_summary(Box::new(io::stdout()), &memory, has_working_ram);
            }
        }
    }

    std::process::exit(error_count);
}
