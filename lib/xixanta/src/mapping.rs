use crate::cfg::{parse_cfg_file, parse_nasm_cfg_file};
use crate::object::Bundle;

const EMPTY_CONFIG: &str = include_str!("mappings/empty.cfg");
const NROM_CONFIG: &str = include_str!("mappings/nrom.cfg");
const NROM65_CONFIG: &str = include_str!("mappings/nrom65.cfg");
const UXROM_CONFIG: &str = include_str!("mappings/unrom.cfg");

/// The type of section that a Mapping represents.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub enum SectionType {
    /// The 16 initial bytes describing the header of the ROM file.
    Header,

    /// Bank to be stored in PRG ROM with a size multiple of 8KB.
    PrgRom,

    /// Bank to be stored in CHR ROM with a size multiple of 4KB.
    ChrRom,
}

/// A segment inside of a memory mapping, used to organize the code inside of a
/// given memory mapping. Note that a segment does not do anything else: it's
/// just about organizing code inside of a mapping. It doesn't deal with how to
/// fill a memory region, or where it starts in memory, or anything like that.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Segment {
    /// Name of the segment.
    pub name: String,

    /// Offset from the base address from where to push the next address for
    /// this segment.
    pub offset: usize,

    /// Bundles that have been generated when assembling the nodes that have
    /// been parsed by a previous step.
    pub bundles: Vec<Bundle>,
}

impl From<&str> for Segment {
    fn from(name: &str) -> Self {
        Segment {
            name: name.to_string(),
            offset: 0,
            bundles: vec![],
        }
    }
}

impl Segment {
    /// Returns the length of the segment by counting the bundles that have been
    // pushed so far into the segment.
    pub fn len(&self) -> usize {
        self.bundles
            .iter()
            .fold(0, |acc, bundle| acc + bundle.size as usize)
    }

    /// Returns true if the given segment has no bundles in it, false otherwise.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// A region in memory which has one or more segments in it, which in turn have
/// the bundles that are to be generated in the end of an assembling operation.
#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Mapping {
    /// Name of the mapping.
    pub name: String,

    /// Address where the mapping will start when loaded on the
    /// console/emulator. This is the address where instructions like `jmp` or
    /// labels will rely on. Hence, it's not the address of the ROM file itself,
    /// but the effective address where it will be loaded.
    pub start: u16,

    /// Size of the mapping. Note that this depends on the `section_type` value,
    /// which is: exactly 0x10 for a header, multiples of 0x4000 for prg_rom,
    /// and multiples of 0x2000 for chr_rom.
    pub size: usize,

    /// The total number of bytes that have been pushed across all its segments.
    pub offset: usize,

    /// Optional value to fill the mapping if the end size is lower than the
    /// value on `size`. Set to `None` to skip filling the memory region for
    /// this mapping.
    pub fill: Option<u8>,

    /// Segments for the memory region.
    pub segments: Vec<Segment>,

    /// What kind of memory region is being described by this mapping.
    pub section_type: SectionType,
}

/// Returns a vector corresponding to the configuration of mappings that is
/// expected for the given `name`. This `name` can either be an already known
/// identifier (e.g. "nrom"), or a file path. Returns an error if the
/// configuration cannot be parsed or there's something wrong about it.
pub fn get_mapping_configuration(name: &str) -> Result<Vec<Mapping>, String> {
    let configuration = if std::fs::exists(name).unwrap_or(false) {
        match std::fs::read_to_string(name) {
            Ok(contents) => {
                // Call the right parse function.
                if contents.starts_with("#!nasmcfg") {
                    parse_nasm_cfg_file(contents.as_str())?
                } else {
                    parse_cfg_file(contents.as_str())?
                }
            }
            Err(_) => return Err(format!("could not read '{}'", name)),
        }
    } else {
        let text = match name.to_lowercase().as_str() {
            "empty" => EMPTY_CONFIG,
            "nrom" => NROM_CONFIG,
            "nrom65" => NROM65_CONFIG,
            "uxrom" | "unrom" => UXROM_CONFIG,
            _ => return Err("mapper configuration is not known".to_string()),
        };
        parse_nasm_cfg_file(text)?
    };

    validate_configuration(&configuration)?;

    Ok(configuration)
}

// Ensure that the given mappings conform to a minimum standard.
fn validate_configuration(mappings: &[Mapping]) -> Result<(), String> {
    if mappings.is_empty() {
        return Err("We need at least one segment defined, the header".to_string());
    }
    if mappings.first().unwrap().segments.is_empty() {
        return Err("We need at least one segment defined, the header".to_string());
    }
    if mappings.first().unwrap().section_type != SectionType::Header {
        return Err("First mapping section must be the header".to_string());
    }
    if mappings.first().unwrap().size != 0x10 {
        return Err("The header must be exactly 16 bytes long".to_string());
    }

    let prg_rom_len = mappings
        .iter()
        .filter(|m| m.section_type == SectionType::PrgRom)
        .fold(0, |acc, x| acc + x.size);

    if prg_rom_len < 0x4000 {
        return Err("PRG ROM must be at least 8KB long".to_string());
    }
    if prg_rom_len % 0x4000 != 0 {
        return Err("PRG ROM must be formed by banks of exactly 8KB".to_string());
    }

    Ok(())
}

/// Perform some sanity checks on the given `mappings`. Only call this function
/// after all bundles have been produced.
pub fn validate(mappings: &[Mapping]) -> Result<(), String> {
    // Guaranteed by `crate::mapping::assert` to be the header.
    let header: &Segment = mappings.first().unwrap().segments.first().unwrap();

    // Header must have at least six bytes with proper information provided by
    // the programmer.
    if header.len() < 6 {
        return Err(String::from("The header must contain at least 6 bytes"));
    }

    // Now check that the length of the evaluated data matches the criteria
    // stated on the ROM header that was evaluated as well.
    let (header_prg_rom_size, header_chr_rom_size) = parse_header(header)?;
    let prg_rom_len = mappings
        .iter()
        .filter(|m| m.section_type == SectionType::PrgRom)
        .fold(0, |acc, x| {
            acc + x.segments.iter().fold(0, |a, y| a + y.len())
        });
    let chr_rom_len = mappings
        .iter()
        .filter(|m| m.section_type == SectionType::ChrRom)
        .fold(0, |acc, x| {
            acc + x.segments.iter().fold(0, |a, y| a + y.len())
        });

    if header_prg_rom_size < prg_rom_len {
        return Err(format!(
            "PRG ROM size is expected to by {} bytes long, but a total of {} bytes were evaluated",
            header_prg_rom_size, prg_rom_len
        ));
    }
    if header_chr_rom_size < chr_rom_len {
        return Err(format!(
            "CHR ROM size is expected to by {} bytes long, but a total of {} bytes were evaluated",
            header_chr_rom_size, chr_rom_len
        ));
    }

    Ok(())
}

// Returns a tuple with the sizes for PRG and CHR ROM as described from the
// computed header. This also does some sanity checks on the header.
fn parse_header(header: &Segment) -> Result<(usize, usize), String> {
    let mut header_it = header.bundles.clone().into_iter();

    // Validate the magic string: 'N', 'E', 'S', $1A
    if header_it.next().unwrap().bytes[0] != b'N' {
        return Err(String::from("First byte of the header must be 'N'"));
    }
    if header_it.next().unwrap().bytes[0] != b'E' {
        return Err(String::from("Second byte of the header must be 'E'"));
    }
    if header_it.next().unwrap().bytes[0] != b'S' {
        return Err(String::from("Third byte of the header must be 'S'"));
    }
    if header_it.next().unwrap().bytes[0] != 26 {
        return Err(String::from(
            "Fourth byte of the header must be the MS-DOS termination character",
        ));
    }

    Ok((
        header_it.next().unwrap().bytes[0] as usize * 0x4000,
        header_it.next().unwrap().bytes[0] as usize * 0x2000,
    ))
}

/// Returns the offset of the segment indexed by `segment_index` inside of
/// `mapping`. That is, it returns back at which byte the given segment is going
/// to be placed inside of the given mapping.
///
/// NOTE: this function is only useful if you already know that all the segments
/// on the given mapping have a definitive size (i.e. they will not change in
/// the future).
pub fn segment_offset(mapping: &Mapping, segment_index: usize) -> u16 {
    let mut count = 0;

    for (idx, segment) in mapping.segments.iter().enumerate() {
        if idx >= segment_index {
            return count;
        }
        count += segment.offset as u16;
    }

    count
}
