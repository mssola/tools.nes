use crate::errors::EvalError;
use crate::object::Bundle;
use toml::{Table, Value};

const EMPTY_CONFIG: &str = include_str!("mappings/empty.toml");
const NROM_CONFIG: &str = include_str!("mappings/nrom.toml");
const NROM65_CONFIG: &str = include_str!("mappings/nrom65.toml");
const UXROM_CONFIG: &str = include_str!("mappings/unrom.toml");

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

    pub offset: usize,
    pub len: usize,

    /// Bundles that have been generated when assembling the nodes that have
    /// been parsed by a previous step.
    pub bundles: Vec<Bundle>,
}

impl From<&str> for Segment {
    fn from(name: &str) -> Self {
        Segment {
            name: name.to_string(),
            offset: 0,
            len: 0,
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
            Ok(contents) => load_configuration_for(contents.as_str())?,
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
        load_configuration_for(text)?
    };

    validate_configuration(&configuration)?;

    Ok(configuration)
}

// Returns the integer value for the mandatory integer contained in `value` that
// is named `prop_name` under the `section_name` section. This integer has to be
// lesser or equal to the `max` value.
fn get_integer(
    section_name: &String,
    prop_name: &str,
    value: Option<&Value>,
    max: usize,
) -> Result<usize, String> {
    if value.is_none() {
        return Err(format!(
            "you have to define a value for '{}' in '{}'",
            section_name, prop_name
        ));
    }
    if !value.unwrap().is_integer() {
        return Err(format!(
            "value for '{}' in '{}' has to be an integer value",
            section_name, prop_name
        ));
    }

    let val = value.unwrap().as_integer().unwrap() as usize;
    if val > max {
        return Err(format!(
            "value for '{}' in '{}' is too big",
            prop_name, section_name
        ));
    }
    Ok(val)
}

// Get a `SectionType` out of the given mandatory `value` which is under the
// `section_name`.
fn parse_section_type(section_name: &String, value: Option<&Value>) -> Result<SectionType, String> {
    match value {
        Some(v) => {
            if !v.is_str() {
                return Err(format!(
                    "'section_type' in '{}' has to be a string",
                    section_name
                ));
            }
            match v.as_str().unwrap().to_lowercase().as_str() {
                "header" => Ok(SectionType::Header),
                "prgrom" => Ok(SectionType::PrgRom),
                "chrrom" => Ok(SectionType::ChrRom),
                _ => Err(format!(
                    "bad value for 'section_type' in '{}'",
                    section_name
                )),
            }
        }
        None => Err(format!(
            "you have to define 'section_type' in '{}'",
            section_name
        )),
    }
}

// Returns a vector of segments which are contained inside of the mandatory
// `value`.
fn get_segments(section_name: &String, value: Option<&Value>) -> Result<Vec<Segment>, String> {
    if value.is_none() {
        return Err(format!(
            "you have to define a value for 'segments' in '{}'",
            section_name
        ));
    }
    if !value.unwrap().is_array() {
        return Err(format!(
            "value for 'segments' in '{}' has to be an array value",
            section_name
        ));
    }

    let mut res = vec![];
    for item in value.unwrap().as_array().unwrap() {
        if !item.is_str() {
            return Err(format!(
                "every item in 'segments' has to be a string ({})",
                section_name
            ));
        }
        res.push(Segment::from(item.as_str().unwrap()));
    }

    Ok(res)
}

// Returns a vector of mappings that is retrieved by parsing the given text.
fn load_configuration_for(text: &str) -> Result<Vec<Mapping>, String> {
    // Obtain the raw data by parsing the given text as a toml::Table.
    let table = match text.parse::<Table>() {
        Ok(t) => t,
        Err(e) => return Err(format!("could not parse configuration file: {}", e)),
    };

    // Each section of the configuration file is a mapping, where the title is
    // simply the name of it.
    let mut mappings = vec![];
    for (name, value) in table {
        let start = get_integer(&name, "start", value.get("start"), u16::MAX as usize)? as u16;
        let size = get_integer(&name, "size", value.get("size"), u16::MAX as usize)?;
        let fill = match value.get("fill") {
            Some(_) => Some(get_integer(&name, "fill", value.get("fill"), u8::MAX as usize)? as u8),
            None => None,
        };
        let section_type = parse_section_type(&name, value.get("section_type"))?;
        let segments = get_segments(&name, value.get("segments"))?;

        mappings.push(Mapping {
            name,
            start,
            size,
            offset: 0,
            fill,
            section_type,
            segments,
        });
    }

    Ok(mappings)
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
pub fn validate(mappings: &[Mapping]) -> Result<(), EvalError> {
    // Guaranteed by `crate::mapping::assert` to be the header.
    let header: &Segment = mappings.first().unwrap().segments.first().unwrap();

    // Header must have at least six bytes with proper information provided by
    // the programmer.
    if header.len() < 6 {
        return Err(EvalError {
            line: 0,
            message: String::from("The header must contain at least 6 bytes"),
            global: true,
        });
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
        return Err(EvalError {
            line: 0,
            message: format!("PRG ROM size is expected to by {} bytes long, but a total of {} bytes were evaluated", header_prg_rom_size, prg_rom_len),
            global: true,
        });
    }
    if header_chr_rom_size < chr_rom_len {
        return Err(EvalError {
            line: 0,
            message: format!("CHR ROM size is expected to by {} bytes long, but a total of {} bytes were evaluated", header_chr_rom_size, chr_rom_len),
            global: true,
        });
    }

    Ok(())
}

// Returns a tuple with the sizes for PRG and CHR ROM as described from the
// computed header. This also does some sanity checks on the header.
fn parse_header(header: &Segment) -> Result<(usize, usize), EvalError> {
    let mut header_it = header.bundles.clone().into_iter();

    // Validate the magic string: 'N', 'E', 'S', $1A
    if header_it.next().unwrap().bytes[0] != b'N' {
        return Err(EvalError {
            line: 0,
            message: String::from("First byte of the header must be 'N'"),
            global: true,
        });
    }
    if header_it.next().unwrap().bytes[0] != b'E' {
        return Err(EvalError {
            line: 0,
            message: String::from("Second byte of the header must be 'E'"),
            global: true,
        });
    }
    if header_it.next().unwrap().bytes[0] != b'S' {
        return Err(EvalError {
            line: 0,
            message: String::from("Third byte of the header must be 'S'"),
            global: true,
        });
    }
    if header_it.next().unwrap().bytes[0] != 26 {
        return Err(EvalError {
            line: 0,
            message: String::from(
                "Fourth byte of the header must be the MS-DOS termination character",
            ),
            global: true,
        });
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
