use header::{Header, Kind};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, ErrorKind, Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use xixanta::mapping::get_mapping_configuration;
use xixanta::opcodes::OPCODES;

/// Version for this program.
const VERSION: &str = "0.1.0";

#[derive(Default)]
struct Args {
    file: String,
    header: bool,
    all: bool,
    disassemble: Option<String>,
    mapping: Option<String>,
    nasm: Option<String>,
    raw: bool,
    config: Option<String>,
}

fn print_help() {
    println!("Display information about NES/Famicom ROM files.\n");
    println!("usage: readrom [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -a, --disassemble-all\tDisassemble everything from the ROM file.");
    println!(
        "  -c, --config <FILE>\tLinker configuration to be used, whether an identifier or a file path."
    );
    println!("  -d, --disassemble <ADDRESS>\tDisassemble starting from the given ADDRESS.");
    println!("  -h, --help\t\t\tPrint this message.");
    println!("  -H, --header\t\t\tJust print the ROM header and quit.");
    println!("  -m, --mapping <NAME>\t\t\tDisassemble the mapped segments as referenced by NAME.");
    println!("  -n, --nasm-directory <PATH>\tPath to the .nasm/ directory.");
    println!("  -r, --raw\t\t\tPrint bytes with no formatting at all when disassembling.");
    println!("  -v, --version\t\t\tPrint the version of this program.");
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
            "-a" | "--all" => res.all = true,
            "-c" | "--config" => match args.next() {
                Some(a) => res.config = Some(a),
                None => die("you need to specify a value for the '-c/--config' flag".to_string()),
            },
            "-d" | "--disassemble" => match args.next() {
                Some(a) => res.disassemble = Some(a),
                None => die(
                    "you need to specify an address for the '-d/--disassemble' flag".to_string(),
                ),
            },
            "-h" | "--help" => print_help(),
            "-H" | "--header" => {
                if res.header {
                    die("do not specify the '-H/--header' flag twice".to_string());
                }
                res.header = true;
            }
            "-m" | "--mapping" => match args.next() {
                Some(a) => res.mapping = Some(a),
                None => {
                    die("you need to specify an address for the '-m/--mapping' flag".to_string())
                }
            },
            "-n" | "--nasm" => match args.next() {
                Some(a) => res.nasm = Some(a),
                None => die("you need to specify a file for the '-n/--nasm' flag".to_string()),
            },
            "-r" | "--raw" => res.raw = true,
            "-v" | "--version" => {
                println!("readrom {VERSION}");
                std::process::exit(0);
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
        die("you need to specify the file to be read".to_string());
    }

    res
}

// Capitalize the given string.
fn capitalize(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn print_header(header: &Header) {
    println!("Header:");

    match header.kind {
        Kind::INes => println!("  Kind:\t\t\tiNES"),
        Kind::Nes20 => println!("  Kind:\t\t\tNES 2.0"),
    }

    println!(
        "  PRG ROM size:\t\t{} bytes ({}KB)",
        header.prg_rom_size * 16 * 1024,
        header.prg_rom_size * 16
    );
    println!(
        "  CHR ROM size:\t\t{} bytes ({}KB)",
        header.chr_rom_size * 8 * 1024,
        header.chr_rom_size * 8
    );

    if let Some(def) = &header.prg_ram_definition {
        println!(
            "  PRG-RAM size:\t\t{} bytes ({}KB); {}",
            def.size,
            def.size / 1024,
            def.kind
        );
    } else if header.has_persistent_memory {
        println!("  PRG-RAM size:\t\t8192 bytes (8KB)");
    }

    if let Some(def) = &header.chr_ram_definition {
        println!(
            "  CHR-RAM size:\t\t{} bytes ({}KB); {}",
            def.size,
            def.size / 1024,
            def.kind
        );
    }

    println!("  Mapper:\t\t{}", header.mapper);
    println!(
        "  Mirroring:\t\t{}",
        capitalize(header.nametable_arrangement.mirroring())
    );
    println!("  CPU/PPU timing:\t{}", header.timing);
}

fn print_vectors(addrs: &[u8]) {
    println!("Vectors:");
    println!(
        "  NMI:\t\t\t{:#04x}",
        u16::from_le_bytes([addrs[0], addrs[1]])
    );
    println!(
        "  Reset:\t\t{:#04x}",
        u16::from_le_bytes([addrs[2], addrs[3]])
    );
    println!(
        "  IRQ:\t\t\t{:#04x}",
        u16::from_le_bytes([addrs[4], addrs[5]])
    );
}

// Print the given `message` and exit(1).
fn die(message: String) {
    println!("error: {message}");
    std::process::exit(1);
}

// Print a block of code from the given open ROM 'file'. The range is indicated
// by 'start' and an optional 'end'. If 'end' is None, then the block of code
// will only span a single instruction. Moreover, the 'memories' and the
// 'addresses' maps can help assist the printing with the information taken from
// the .nasm/memory.txt and .nasm/addresses.txt files respectively. Finally, set
// 'raw' to true if you want all bytes to be printed directly into the stdout,
// otherwise a human-readable format will be used.
fn print_range(
    mut file: &File,
    start: usize,
    end: Option<usize>,
    memories: HashMap<usize, String>,
    addresses: HashMap<usize, String>,
    raw: bool,
    filter: Option<&str>,
) -> Result<(), String> {
    let mut bytes = Vec::new();
    file.seek(SeekFrom::Start(0)).map_err(|e| e.to_string())?;
    file.read_to_end(&mut bytes).map_err(|e| e.to_string())?;

    // Fetch the bytes to be printed.

    // NOTE: minus 0x8000 to account for non-ROM address, plus 0x10 to skip the
    // header from the file.
    let range_start = start
        .checked_sub(0x8000)
        .map(|val| val + 0x10)
        .ok_or_else(|| format!("bad start address {:#x}", start))?;

    // The 'end' of the range depends on whether the user is just printing a
    // single instruction or it's really trying to print a proper range.
    let res = match end {
        Some(e) => {
            let range_end = e
                .checked_sub(0x8000)
                .map(|val| val + 0x10)
                .ok_or_else(|| format!("bad end address {:#x}", e))?;
            bytes.get(range_start..range_end).unwrap_or(&[])
        }
        // If it's just one instruction, take the opcode byte and some more to
        // account for the maximum size of an instruction on this platform.
        None => bytes.get(range_start..range_start + 3).unwrap_or(&[]),
    };

    // Printing raw: blindessly spit bytes to stdout.
    if raw {
        let mut writer = BufWriter::new(std::io::stdout());
        writer
            .write_all(&res[..res.len()])
            .map_err(|_| "cannot write to the stdout".to_string())?;
        return Ok(());
    }

    // Print into a more human-readable shape.

    let mut iter = res.iter();
    let mut current_address = start;
    while let Some(byte) = iter.next() {
        // Given the opcode, fetch the instruction object for it, and how much
        // the address should be advanced after printing the instruction.
        let (instr, size, formatted) = match OPCODES.get(byte) {
            Some(tpl) => {
                let mut ins = tpl.clone();
                let mut formatted = format!("{:02X}\t", byte);

                match ins.size {
                    2 => {
                        ins.bytes[0] = *iter.next().unwrap_or(&0);
                        formatted = format!("{:02X} {:02X}\t", byte, ins.bytes[0]);
                    }
                    3 => {
                        ins.bytes[0] = *iter.next().unwrap_or(&0);
                        ins.bytes[1] = *iter.next().unwrap_or(&0);
                        formatted =
                            format!("{:02X} {:02X} {:02X}", byte, ins.bytes[0], ins.bytes[1]);
                    }
                    _ => {}
                }

                (
                    ins.to_human(current_address, filter, &memories, &addresses),
                    ins.size,
                    formatted,
                )
            }
            None => ("<unknown>".to_string(), 1, "<unknown>".to_string()),
        };

        // Do we actually know of a label which points to the current address?
        // If so, show it now.
        if let Some(address_name) = addresses.get(&current_address)
            && current_address != start
        {
            match filter {
                Some(_) => println!(
                    "\n  {}:",
                    address_name.split("::").last().unwrap_or(address_name)
                ),
                None => println!("\n  {}:", address_name),
            }
        }

        // And print our awesome line :)
        println!("${:4X}:\t{}\t{} ", current_address, formatted, instr);
        current_address += size as usize;

        if end.is_none() {
            break;
        }
    }

    // Sometimes there is a label marking the end of the code, which is set
    // after the last instruction. Show these labels too as some branch
    // instructions can use it.
    if let Some(address_name) = addresses.get(&current_address) {
        match filter {
            Some(_) => println!(
                "\n  {}:",
                address_name.split("::").last().unwrap_or(address_name)
            ),
            None => println!("\n  {}:", address_name),
        }
    }

    Ok(())
}

fn parse_hex_value(address: &str) -> Option<usize> {
    match address.len() {
        // Simple 'a2fb' format.
        4 => {
            if let Ok(val) = usize::from_str_radix(address, 16) {
                return Some(val);
            }
        }
        // nasm's '$a2fb' format.
        5 => {
            if let Some(addr) = address.get(1..)
                && let Ok(val) = usize::from_str_radix(addr, 16)
            {
                return Some(val);
            }
        }
        // Standard '0xa2fb' format.
        6 => {
            if let Some(addr) = address.get(2..)
                && let Ok(val) = usize::from_str_radix(addr, 16)
            {
                return Some(val);
            }
        }
        _ => {}
    }

    None
}

fn do_disassemble(
    input: &File,
    address: Option<&str>,
    nasm_path: &Option<String>,
    mut start: Option<usize>,
    mut end: Option<usize>,
    raw: bool,
) -> Result<(), String> {
    let mut is_nasm_path = true;
    let mut addresses: HashMap<usize, String> = HashMap::default();
    let mut memories: HashMap<usize, String> = HashMap::default();

    // Fill up the 'addresses' and the 'memories' maps.
    if let Some(path) = nasm_path {
        if let Ok(file) = File::open(PathBuf::from(path).join("addresses.txt")) {
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let line = line.map_err(|e| e.to_string())?;
                let columns: Vec<&str> = line.split(',').map(|s| s.trim()).collect();
                if columns.len() != 3 {
                    return Err("badly formatted address file".to_string());
                }

                let parsed_start = usize::from_str_radix(columns[1], 16)
                    .map_err(|_| format!("invalid hex value: '{}'", columns[1]))?;
                addresses.insert(parsed_start, columns[0].to_string());

                if start.is_none() && columns[0] == address.unwrap() {
                    start = Some(parsed_start);
                    end = Some(
                        usize::from_str_radix(columns[2], 16)
                            .map_err(|_| format!("invalid hex value: '{}'", columns[2]))?,
                    );
                }
            }
        }

        // If the memory.txt file is available, fill up the 'memories' hash.
        if let Ok(file) = File::open(PathBuf::from(path).join("memory.txt")) {
            let reader = BufReader::new(file);
            for line in reader.lines() {
                let line = line.map_err(|e| e.to_string())?;
                if line.is_empty() || line.starts_with("---") {
                    break;
                }

                let (left, right) = line.split_once(':').unwrap();
                let start = match left.trim().split_once('-') {
                    Some((start, _)) => usize::from_str_radix(start.get(1..).unwrap(), 16).unwrap(),
                    None => usize::from_str_radix(left.get(1..).unwrap(), 16).unwrap(),
                };
                memories.insert(start, right.trim().to_string());
            }
        }
    } else {
        is_nasm_path = false;
    }

    // If this is just a numeric value, take it as is.
    if let Some(address) = address
        && let Some(start) = parse_hex_value(address)
    {
        return print_range(input, start, None, memories, addresses, raw, None);
    }

    // Otherwise, print the full range if possible.
    if !is_nasm_path {
        Err("you need to use the '-n/--nasm-directory' on disassembly".to_string())
    } else if addresses.is_empty() {
        Err("failed to open the .nasm/addresses.txt file".to_string())
    } else {
        let filter_string: Option<String> = address.map(|addr| format!("{addr}::"));
        let filter: Option<&str> = filter_string.as_deref();

        match start {
            Some(s) => print_range(
                input,
                s,
                Some(end.unwrap()),
                memories,
                addresses,
                raw,
                filter,
            ),
            None => Err(format!("could not find address '{}'", address.unwrap())),
        }
    }
}

fn handle_disassembling_args(args: &Args, input: &File) -> Result<bool, String> {
    if let Some(address) = &args.disassemble {
        if let Err(e) = do_disassemble(input, Some(address), &args.nasm, None, None, args.raw) {
            die(e);
        }
        return Ok(true);
    }

    if args.all {
        match &args.config {
            Some(cfg) => {
                let mappings = get_mapping_configuration(cfg)?;
                for m in &mappings {
                    // If this is not code, then just skip it.
                    if m.start < 0x8000 {
                        continue;
                    }

                    let start = m.start as usize;
                    let end = start + m.size;
                    println!(
                        "\n=> Start of '{}', which contains these segments: {}.\n",
                        m.name,
                        m.segments
                            .iter()
                            .map(|s| s.name.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    do_disassemble(input, None, &args.nasm, Some(start), Some(end), args.raw)?;
                }
            }
            None => {
                return Err(
                    "you need to provide the configuration file with '-c/--config'".to_string(),
                );
            }
        }
        return Ok(true);
    }

    if let Some(name) = &args.mapping {
        match &args.config {
            Some(cfg) => {
                let mappings = get_mapping_configuration(cfg)?;
                for m in &mappings {
                    if m.name == *name {
                        let start = m.start as usize;
                        let end = start + m.size;
                        println!(
                            "\n=> Start of '{}', which contains these segments: {}.\n",
                            *name,
                            m.segments
                                .iter()
                                .map(|s| s.name.clone())
                                .collect::<Vec<_>>()
                                .join(", ")
                        );
                        do_disassemble(input, None, &args.nasm, Some(start), Some(end), args.raw)?;
                        return Ok(true);
                    }
                    for segment in &m.segments {
                        if segment.name == *name {
                            let start = m.start as usize;
                            let end = start + m.size;
                            println!(
                                "\n=> Start of '{}', which contains these segments: {}.\n",
                                *name,
                                m.segments
                                    .iter()
                                    .map(|s| s.name.clone())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            );
                            do_disassemble(
                                input,
                                None,
                                &args.nasm,
                                Some(start),
                                Some(end),
                                args.raw,
                            )?;
                            return Ok(true);
                        }
                    }
                }
            }
            None => {
                return Err(
                    "you need to provide the configuration file with '-c/--config'".to_string(),
                );
            }
        }
        return Ok(true);
    }

    Ok(false)
}

fn main() {
    let args = parse_arguments();

    let Ok(mut input) = File::open(&args.file) else {
        die(format!("failed to open the given file '{}'", args.file));
        return;
    };

    // Check whether the user wanted to disassemble something.
    match handle_disassembling_args(&args, &input) {
        Ok(quit) => {
            if quit {
                std::process::exit(0);
            }
        }
        Err(e) => die(e),
    }

    // Nope. Then let's just print information about it. First the header.

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

    if args.header {
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
