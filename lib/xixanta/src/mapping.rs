use crate::assembler::Bundle;
use crate::errors::EvalError;

lazy_static! {
    /// An empty mapper used for testing purposes.
    pub static ref EMPTY: Vec<Mapping> = vec![
        Mapping {
            name: String::from("HEADER"),
            start: 0x0000,
            size: 0x0010,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::Header,
            segments: vec![Segment {
                name: String::from("HEADER"),
                len: 0,
                offset: 0,
                bundles: vec![],
            }]
        },
        Mapping {
            name: String::from("ROM0"),
            start: 0x8000,
            size: 0x8000,
            offset: 0,
            fill: None,
            section_type: SectionType::PrgRom,
            segments: vec![Segment {
                name: String::from("CODE"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
    ];

    // Mapper for a simple NROM setup (e.g. Super Mario Bros).
    pub static ref NROM: Vec<Mapping> = vec![
        Mapping {
            name: String::from("HEADER"),
            start: 0x0000,
            size: 0x0010,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::Header,
            segments: vec![Segment {
                name: String::from("HEADER"),
                len: 0,
                offset: 0,
                bundles: vec![],
            }]
        },
        Mapping {
            name: String::from("ROM0"),
            start: 0x8000,
            size: 0x7FFA,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::PrgRom,
            segments: vec![Segment {
                name: String::from("CODE"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
        Mapping {
            name: String::from("ROMV"),
            start: 0xFFFA,
            size: 0x0006,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::PrgRom,
            segments: vec![Segment {
                name: String::from("VECTORS"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
        Mapping {
            name: String::from("ROM2"),
            start: 0x0000,
            size: 0x2000,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::ChrRom,
            segments: vec![Segment {
                name: String::from("CHARS"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
    ];

    // The same mapper as NROM, but it adds a "STARTUP" segment into the "ROM0"
    // mapping so to behave the same as the default "cc65" configuration.
    pub static ref NROM65: Vec<Mapping> = vec![
        Mapping {
            name: String::from("HEADER"),
            start: 0x0000,
            size: 0x0010,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::Header,
            segments: vec![Segment {
                name: String::from("HEADER"),
                len: 0,
                offset: 0,
                bundles: vec![],
            }]
        },
        Mapping {
            name: String::from("ROM0"),
            start: 0x8000,
            size: 0x7FFA,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::PrgRom,
            segments: vec![
                Segment {
                    name: String::from("STARTUP"),
                    len: 0,
                    offset: 0,
                    bundles: vec![],
                },
                Segment {
                    name: String::from("CODE"),
                    len: 0,
                    offset: 0,
                    bundles: vec![],
                },
            ]
        },
        Mapping {
            name: String::from("ROMV"),
            start: 0xFFFA,
            size: 0x0006,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::PrgRom,
            segments: vec![Segment {
                name: String::from("VECTORS"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
        Mapping {
            name: String::from("ROM2"),
            start: 0x0000,
            size: 0x2000,
            offset: 0,
            fill: Some(0x00),
            section_type: SectionType::ChrRom,
            segments: vec![Segment {
                name: String::from("CHARS"),
                len: 0,
                offset: 0,
                bundles: vec![],
            },]
        },
    ];
}

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

/// Assert that the given mappings conform to a minimum standard.
pub fn assert(mappings: &[Mapping]) {
    assert!(
        !mappings.is_empty(),
        "We need at least one segment defined, the header"
    );
    assert!(
        !mappings.first().unwrap().segments.is_empty(),
        "We need at least one segment defined, the header"
    );
    assert_eq!(
        mappings.first().unwrap().section_type,
        SectionType::Header,
        "First mapping section must be the header"
    );
    assert_eq!(
        mappings.first().unwrap().size,
        0x10,
        "The header must be exactly 16 bytes long"
    );

    let prg_rom_len = mappings
        .iter()
        .filter(|m| m.section_type == SectionType::PrgRom)
        .fold(0, |acc, x| acc + x.size);
    assert!(prg_rom_len >= 0x4000, "PRG ROM must be at least 8KB long");
    assert!(
        prg_rom_len % 0x4000 == 0,
        "PRG ROM must be formed by banks of exactly 8KB"
    );
}

/// Validate some sanity checks on the given `mappings`. Only call this function
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
