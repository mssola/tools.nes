/// ROM header formats.
#[derive(Debug)]
pub enum Kind {
    /// Historical header used since the iNES emulator.
    INes,

    /// Modern header which is backwards-compatible in regards to iNES. Using
    /// this header is helpful to disambiguate certain mappers or to be more
    /// specific about some of them (e.g. submappers inside of MMC3).
    Nes20,
}

/// Nametable arrangement as defined by the header. Typically it's either
/// Vertical or Horizontal, since this library does not support "alternative"
/// types. Hence, an "Unknown" value is most probably either a defect on the ROM
/// file, or a weird alternative arrangement what we don't care.
#[derive(Debug)]
pub enum NameTableArrangement {
    Vertical,
    Horizontal,
    Unknown,
}

/// Memory mappers. Note that not all of them are listed here, but they will be
/// added with time (and if I even care).
#[derive(Debug)]
pub enum Mapper {
    Axrom,
    BnromCombo,
    BnromOnly,
    Cnrom,
    Mmc1,
    Mmc2,
    Mmc3Acc,
    Mmc3c,
    Mmc3Nec,
    Mmc3Sharp,
    Mmc3T9552,
    Mmc4,
    Mmc5,
    Mmc6,
    Nina001,
    Nrom,
    Unknown,
    Unrom512,
    Uxrom,
}

impl std::fmt::Display for Mapper {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Mapper::Axrom => write!(f, "AxROM"),
            Mapper::BnromCombo => write!(f, "BNROM / NINA-001"),
            Mapper::BnromOnly => write!(f, "BNROM"),
            Mapper::Cnrom => write!(f, "CNROM"),
            Mapper::Mmc1 => write!(f, "MMC1"),
            Mapper::Mmc2 => write!(f, "MMC2"),
            Mapper::Mmc3Acc => write!(f, "MC-ACC"),
            Mapper::Mmc3c => write!(f, "MMC3C"),
            Mapper::Mmc3Nec => write!(f, "MMC3 (NEC)"),
            Mapper::Mmc3Sharp => write!(f, "MMC3 (Sharp)"),
            Mapper::Mmc3T9552 => write!(f, "MMC3 (variant with a T9552 scrambling chip)"),
            Mapper::Mmc4 => write!(f, "MMC4"),
            Mapper::Mmc5 => write!(f, "MMC5"),
            Mapper::Mmc6 => write!(f, "MMC6"),
            Mapper::Nina001 => write!(f, "NINA 001"),
            Mapper::Nrom => write!(f, "NROM"),
            Mapper::Unknown => write!(f, "unknown"),
            Mapper::Unrom512 => write!(f, "UNROM 512"),
            Mapper::Uxrom => write!(
                f,
                "UxROM (NES-UNROM, NES-UOROM, HVC-UN1ROM their HVC counterparts, and clone boards)"
            ),
        }
    }
}

/// Information that has been parsed from an NES/Famicom ROM header. The
/// `Header` struct implements the `TryFrom` trait. Hence, use
/// `Header::try_from` in order to parse a header.
#[derive(Debug)]
pub struct Header {
    /// PRG ROM size in units of 16KB. That is, a "1" here actually means
    /// "16KB". This is in accordance to the header itself, but callers that
    /// need to display this information should perform the translation to be
    /// more useful to the human eye.
    pub prg_rom_size: usize,

    /// CHR ROM size in units of 8KB. That is, a "1" here actually means "8KB".
    /// This is in accordance to the header itself, but callers that need to
    /// display this information should perform the translation to be more
    /// useful to the human eye.
    pub chr_rom_size: usize,

    /// Nametable arrangement.
    pub nametable_arrangement: NameTableArrangement,

    /// Whether the memory region $6000-$7FFF is persistent or not (e.g.
    /// battery-backed and handled through a mapper like MMC1).
    pub has_persistent_memory: bool,

    /// Whether there is a 512-byte trainer at $7000-$71FF or not.
    pub has_trainer: bool,

    /// The memory mapper being used.
    pub mapper: Mapper,

    /// The kind of the header.
    pub kind: Kind,
}

impl TryFrom<&[u8]> for Header {
    type Error = &'static str;

    fn try_from(bytes: &[u8]) -> Result<Self, Self::Error> {
        let kind = get_rom_kind(bytes)?;
        let ninth = bytes.get(9).unwrap_or(&0);

        Ok(Self {
            prg_rom_size: if matches!(kind, Kind::Nes20) {
                (((ninth & 0x0F) as usize) << 8) + bytes[4] as usize
            } else {
                bytes[4] as usize
            },
            chr_rom_size: if matches!(kind, Kind::Nes20) {
                (((ninth & 0xF0) as usize) << 4) + bytes[5] as usize
            } else {
                bytes[5] as usize
            },
            nametable_arrangement: parse_nametable(bytes.get(6)),
            has_persistent_memory: (bytes.get(6).unwrap_or(&0) & 0x2) == 0x2,
            has_trainer: (bytes.get(6).unwrap_or(&0) & 0x4) == 0x4,
            mapper: parse_mapper(bytes.get(6), bytes.get(7), bytes.get(8)),
            kind,
        })
    }
}

fn parse_nametable(byte: Option<&u8>) -> NameTableArrangement {
    match byte {
        Some(b) => {
            if b & 0x1 == 0 {
                NameTableArrangement::Vertical
            } else {
                NameTableArrangement::Horizontal
            }
        }
        None => NameTableArrangement::Unknown,
    }
}

fn parse_mapper(sixth: Option<&u8>, seventh: Option<&u8>, eighth: Option<&u8>) -> Mapper {
    match sixth {
        Some(l) => {
            let byte = (l & 0xF0) >> 4;
            let s = seventh.unwrap_or(&0);
            let result = (s & 0xF0) | byte;

            // Is this NES 2.0 format? If so, then the eighth byte will contain
            // further information on mapper/submapper.
            if (s & 0x0C) == 0x08 {
                let e = eighth.unwrap_or(&0);
                get_mapper_from_id(
                    ((*e as usize & 0x0F) << 16) + result as usize,
                    (e & 0xF0) >> 4,
                )
            } else {
                get_mapper_from_id(result as usize, 0)
            }
        }
        None => Mapper::Unknown,
    }
}

fn get_mapper_from_id(mapper_id: usize, submapper_id: u8) -> Mapper {
    match mapper_id {
        0 => Mapper::Nrom,
        1 => Mapper::Mmc1,
        2 => Mapper::Uxrom,
        3 => Mapper::Cnrom,
        4 => match submapper_id {
            0 => Mapper::Mmc3Sharp,
            1 => Mapper::Mmc6,
            2 => Mapper::Mmc3c,
            3 => Mapper::Mmc3Acc,
            4 => Mapper::Mmc3Nec,
            5 => Mapper::Mmc3T9552,
            _ => Mapper::Unknown,
        },
        5 => Mapper::Mmc5,
        7 => Mapper::Axrom,
        9 => Mapper::Mmc2,
        10 => Mapper::Mmc4,
        30 => Mapper::Unrom512,
        34 => match submapper_id {
            0 => Mapper::BnromCombo,
            1 => Mapper::BnromOnly,
            2 => Mapper::Nina001,
            _ => Mapper::Unknown,
        },
        _ => Mapper::Unknown,
    }
}

/// Get the Kind as parsed from the header expressed in `bytes`.
pub fn get_rom_kind(bytes: &[u8]) -> Result<Kind, &'static str> {
    if bytes.len() < 6 {
        return Err("given header is too short");
    }
    if bytes[0] != b'N' || bytes[1] != b'E' || bytes[2] != b'S' || bytes[3] != 0x1A {
        return Err("invalid magic value for header");
    }

    if (bytes.get(7).unwrap_or(&0) & 0x0C) == 0x08 {
        Ok(Kind::Nes20)
    } else {
        Ok(Kind::INes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_rom_kind_test() {
        assert_eq!(
            get_rom_kind(vec![].as_slice()).unwrap_err(),
            "given header is too short"
        );

        assert_eq!(
            get_rom_kind(vec![b'N', b'E', b'S', b'\0', 0x01, 0x00].as_slice()).unwrap_err(),
            "invalid magic value for header"
        );

        // iNES without the seventh byte.
        assert!(matches!(
            get_rom_kind(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x00, 0x00].as_slice()).unwrap(),
            Kind::INes
        ));

        // iNES with seventh byte.
        assert!(matches!(
            get_rom_kind(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x00, 0x00, 0x00].as_slice()).unwrap(),
            Kind::INes
        ));

        assert!(matches!(
            get_rom_kind(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x00, 0x00, 0x08].as_slice()).unwrap(),
            Kind::Nes20
        ));
    }

    #[test]
    fn nrom_test() {
        let mut header =
            Header::try_from(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x00, 0x00].as_slice())
                .unwrap();
        assert_eq!(header.prg_rom_size, 1);
        assert_eq!(header.chr_rom_size, 1);
        assert!(matches!(
            header.nametable_arrangement,
            NameTableArrangement::Vertical
        ));
        assert!(!header.has_persistent_memory);
        assert!(!header.has_trainer);
        assert!(matches!(header.mapper, Mapper::Nrom));
        assert!(matches!(header.kind, Kind::INes));

        header = Header::try_from(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x00, 0x08].as_slice())
            .unwrap();
        assert_eq!(header.prg_rom_size, 1);
        assert_eq!(header.chr_rom_size, 1);
        assert!(matches!(
            header.nametable_arrangement,
            NameTableArrangement::Vertical
        ));
        assert!(!header.has_persistent_memory);
        assert!(!header.has_trainer);
        assert!(matches!(header.mapper, Mapper::Nrom));
        assert!(matches!(header.kind, Kind::Nes20));
    }

    #[test]
    fn mmc3_mmc6_test() {
        // MMC3 Sharp in iNES format.
        let mut header =
            Header::try_from(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x40, 0x00].as_slice())
                .unwrap();
        assert_eq!(header.prg_rom_size, 1);
        assert_eq!(header.chr_rom_size, 1);
        assert!(matches!(
            header.nametable_arrangement,
            NameTableArrangement::Vertical
        ));
        assert!(!header.has_persistent_memory);
        assert!(!header.has_trainer);
        assert!(matches!(header.mapper, Mapper::Mmc3Sharp));
        assert!(matches!(header.kind, Kind::INes));

        // MMC3 Sharp but with NES 2.0 (notice explicit 0 submapper).
        header =
            Header::try_from(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x40, 0x08, 0x00].as_slice())
                .unwrap();
        assert_eq!(header.prg_rom_size, 1);
        assert_eq!(header.chr_rom_size, 1);
        assert!(matches!(
            header.nametable_arrangement,
            NameTableArrangement::Vertical
        ));
        assert!(!header.has_persistent_memory);
        assert!(!header.has_trainer);
        assert!(matches!(header.mapper, Mapper::Mmc3Sharp));
        assert!(matches!(header.kind, Kind::Nes20));

        // Disambiguate to MMC6 thanks to submapper
        header =
            Header::try_from(vec![b'N', b'E', b'S', 0x1A, 0x01, 0x01, 0x40, 0x08, 0x10].as_slice())
                .unwrap();
        assert_eq!(header.prg_rom_size, 1);
        assert_eq!(header.chr_rom_size, 1);
        assert!(matches!(
            header.nametable_arrangement,
            NameTableArrangement::Vertical
        ));
        assert!(!header.has_persistent_memory);
        assert!(!header.has_trainer);
        assert!(matches!(header.mapper, Mapper::Mmc6));
        assert!(matches!(header.kind, Kind::Nes20));
    }
}
