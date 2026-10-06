use header::{Header, Kind};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use xixanta::mapping::get_mapping_configuration;
use xixanta::opcodes::OPCODES;

/// Version for this program.
const VERSION: &str = "0.1.0";

// Required configuration for the MMC3 chip.
struct Mmc3 {
    prg_rom_mode: bool,
    r6: usize,
    r7: usize,
}

// Accepted parameters for the '--mmc' flag.
enum MemoryMapperChip {
    Mmc3(Mmc3),
}

#[derive(Default)]
struct Args {
    file: String,
    header: bool,
    all: bool,
    disassemble: Option<String>,
    mapping: Option<String>,
    nasm: Option<PathBuf>,
    raw: bool,
    mmc: Option<MemoryMapperChip>,
    config: Option<String>,
}

fn print_help() {
    println!("Display information about NES/Famicom ROM files.\n");
    println!("usage: readrom [OPTIONS] <FILE>\n");
    println!("Options:");
    println!("  -a, --disassemble-all\t\tDisassemble everything from the ROM file.");
    println!(
        "  -c, --config <FILE>\t\tLinker configuration to be used, whether an identifier or a file path."
    );
    println!("  -d, --disassemble <ADDRESS>\tDisassemble starting from the given ADDRESS.");
    println!("  -h, --help\t\t\tPrint this message.");
    println!("  -H, --header\t\t\tJust print the ROM header and quit.");
    println!("  -m, --mapping <NAME>\t\tDisassemble the mapped segments as referenced by NAME.");
    println!("  --mmc <CONFIG>\t\tProvide the configuration for a memory mapper chip.");
    println!("  -n, --nasm <PATH>\t\tPath to the .nasm/ directory.");
    println!("  -r, --raw\t\t\tPrint bytes with no formatting at all when disassembling.");
    println!("  -v, --version\t\t\tPrint the version of this program.");
    std::process::exit(0);
}

// Given an option line, parse it in order to fetch the configuration for a
// memory mapper chip. If something goes wrong, it will simply call die().
fn parse_mmc(line: &str) -> MemoryMapperChip {
    let map: HashMap<String, String> = line
        .split(';')
        .filter_map(|pair| pair.split_once('='))
        .map(|(k, v)| (k.trim().to_lowercase(), v.trim().to_lowercase()))
        .collect();

    match map.get("kind").map(|s| s.as_str()) {
        Some("mmc3") => {
            let parse_field = |key: &str| -> usize {
                map.get(key)
                    .unwrap_or_else(|| {
                        die(format!("missing required field '{key}' in '--mmc' flag"))
                    })
                    .parse::<usize>()
                    .unwrap_or_else(|_| die(format!("invalid integer for '{key}' in '--mmc' flag")))
            };

            MemoryMapperChip::Mmc3(Mmc3 {
                prg_rom_mode: parse_field("prg_rom_mode") == 1,
                r6: parse_field("r6"),
                r7: parse_field("r7"),
            })
        }
        Some(mmc) => die(format!(
            "unknown memory mapper kind '{mmc}' in the '--mmc' flag"
        )),
        None => die("you need to specify the 'kind' in the '--mmc' flag".to_string()),
    }
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
            "--mmc" => match args.next() {
                Some(c) => res.mmc = Some(parse_mmc(&c)),
                None => die("you need to specify a configuration for the '--mmc' flag".to_string()),
            },
            "-n" | "--nasm" => match args.next() {
                Some(a) => {
                    let pb = PathBuf::from(a.clone());
                    if !pb.exists() {
                        die(format!("directory '{a}' does not exist"));
                    }
                    if !pb.is_dir() {
                        die(format!("path '{a}' does not point to a directory"));
                    }
                    res.nasm = Some(pb);
                }
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
    if addrs.is_empty() {
        println!("  Could not retrieve information on this ROM (bad file?)");
        return;
    }

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
fn die(message: String) -> ! {
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
    bytes: &[u8],
    start: usize,
    end: Option<usize>,
    memories: HashMap<usize, String>,
    addresses: HashMap<usize, String>,
    raw: bool,
    filter: Option<&str>,
) -> Result<(), String> {
    // Put a lower and upper bounds to the bytes to be used for printing.

    // NOTE: minus 0x8000 to account for non-ROM address.
    let range_start = start
        .checked_sub(0x8000)
        .ok_or_else(|| format!("bad start address {:#x}", start))?;

    // The 'end' of the range depends on whether the user is just printing a
    // single instruction or it's really trying to print a proper range.
    let res = match end {
        Some(e) => {
            let range_end = e
                .checked_sub(0x8000)
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
    bytes: &[u8],
    address: Option<&str>,
    nasm_path: &Option<PathBuf>,
    mut start: Option<usize>,
    mut end: Option<usize>,
    raw: bool,
) -> Result<(), String> {
    let mut is_nasm_path = true;
    let mut addresses: HashMap<usize, String> = HashMap::default();
    let mut memories: HashMap<usize, String> = HashMap::default();

    // Fill up the 'addresses' and the 'memories' maps.
    if let Some(path) = nasm_path {
        if let Ok(file) = File::open(path.join("addresses.txt")) {
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
        return print_range(bytes, start, None, memories, addresses, raw, None);
    }

    // Otherwise, print the full range if possible.
    if !is_nasm_path {
        Err("you need to use the '-n/--nasm' on disassembly".to_string())
    } else if addresses.is_empty() {
        Err("failed to open the .nasm/addresses.txt file".to_string())
    } else {
        let filter_string: Option<String> = address.map(|addr| format!("{addr}::"));
        let filter: Option<&str> = filter_string.as_deref();

        match start {
            Some(s) => print_range(
                bytes,
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

fn handle_disassembling_args(args: &Args, bytes: &[u8]) -> Result<bool, String> {
    if let Some(address) = &args.disassemble {
        if let Err(e) = do_disassemble(bytes, Some(address), &args.nasm, None, None, args.raw) {
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
                    do_disassemble(bytes, None, &args.nasm, Some(start), Some(end), args.raw)?;
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
                        do_disassemble(bytes, None, &args.nasm, Some(start), Some(end), args.raw)?;
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
                                bytes,
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

// Returns the same 'bytes' but reconstructed so it fits into the banking
// configuration established in 'mmc'.
fn reconstruct_from_mmc3(header: &Header, mmc: &Mmc3, bytes: &[u8]) -> Result<Vec<u8>, String> {
    // We first grab the bytes for PRG ROM.
    let rom_size = header.prg_rom_size * 16 * 1024;
    let prg_rom = bytes.get(0x10..(rom_size + 0x10)).unwrap_or(&[]);
    if prg_rom.is_empty() {
        return Err("malformed ROM file: PRG ROM is empty".to_string());
    }

    // We split the PRG ROM into 8KB chunks, which are the base unit for PRG ROM
    // bank switching. If PRG ROM cannot be divided into these chunks, there's
    // something off with the ROM file.
    let (chunks, remainder) = prg_rom.as_chunks::<{ 8 * 1024 }>();
    if !remainder.is_empty() {
        return Err("malformed ROM file: PRG ROM size is not a multiple of 8KB".to_string());
    }

    // Pair each chunk with the proper identifier.
    let bank_r6 = *chunks.get(mmc.r6).ok_or("mmc3: invalid R6 bank index")?;
    let bank_r7 = *chunks.get(mmc.r7).ok_or("mmc3: invalid R7 bank index")?;
    let bank_second_last = *chunks
        .get(chunks.len().saturating_sub(2))
        .ok_or("mmc3: PRG ROM too small")?;
    let bank_last = *chunks
        .get(chunks.len().saturating_sub(1))
        .ok_or("mmc3: PRG ROM too small")?;

    // And then, according to the established PRG ROM mode, let's sort each
    // register as expected by the MMC3 chip and push it into the vector we end
    // up returning.
    let mut ret = Vec::with_capacity(rom_size);
    if mmc.prg_rom_mode {
        ret.extend_from_slice(&bank_second_last);
        ret.extend_from_slice(&bank_r7);
        ret.extend_from_slice(&bank_r6);
        ret.extend_from_slice(&bank_last);
    } else {
        ret.extend_from_slice(&bank_r6);
        ret.extend_from_slice(&bank_r7);
        ret.extend_from_slice(&bank_second_last);
        ret.extend_from_slice(&bank_last);
    }

    Ok(ret)
}

fn main() {
    let args = parse_arguments();

    // Open the ROM file and read it.
    let Ok(mut input) = File::open(&args.file) else {
        die(format!("failed to open the given file '{}'", args.file));
    };
    let mut bytes = Vec::new();
    if let Err(e) = input.read_to_end(&mut bytes) {
        die(e.to_string());
    }

    let buf = bytes.get(0..0x10).unwrap_or(&[]);
    if buf.is_empty() {
        die("malformed ROM file".to_string());
    }

    let header = match Header::try_from(buf) {
        Ok(h) => h,
        Err(e) => die(e.to_string()),
    };

    // If the ROM file is using a special mapper (e.g. MMC3), it may provide a
    // memory mapper chip configuration. If that's the case, then 'bytes' might
    // need to be re-constructed with the given mapper configuration. Otherwise,
    // we remove the header part from the ROM, as it's no longer needed.
    bytes = match &args.mmc {
        Some(MemoryMapperChip::Mmc3(m)) => match reconstruct_from_mmc3(&header, m, &bytes) {
            Ok(b) => b,
            Err(e) => die(e),
        },
        None => bytes.get(0x10..).unwrap_or(&[]).to_vec(),
    };

    // Check whether the user wanted to disassemble something.
    match handle_disassembling_args(&args, &bytes) {
        Ok(quit) => {
            if quit {
                std::process::exit(0);
            }
        }
        Err(e) => die(e),
    }

    // Nope. Then let's just print information about it. First the header.

    print_header(&header);
    if args.header {
        std::process::exit(0);
    }

    // Vectors.

    let vectors = &bytes.get(0x7FFA..).unwrap_or(&[]);
    print_vectors(vectors);
}
