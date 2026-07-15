use crate::node::{ControlType, EchoKind};
use std::collections::HashMap;
use std::fmt;
use std::sync::LazyLock;

/// Represents the addressing mode being used by an instruction.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum AddressingMode {
    Implied,
    Immediate,
    Absolute,
    RelativeOrZeropage,
    IndexedX,
    IndexedY,
    ZeropageIndexedX,
    ZeropageIndexedY,
    Indirect,
    IndirectX,
    IndirectY,
}

impl fmt::Display for AddressingMode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AddressingMode::Implied => write!(f, "implied"),
            AddressingMode::Immediate => write!(f, "immediate"),
            AddressingMode::Absolute => write!(f, "absolute"),
            AddressingMode::RelativeOrZeropage => write!(f, "relative or zeropage"),
            AddressingMode::IndexedX => write!(f, "indexed by x"),
            AddressingMode::IndexedY => write!(f, "indexed by y"),
            AddressingMode::ZeropageIndexedX => write!(f, "zeropage indexed by x"),
            AddressingMode::ZeropageIndexedY => write!(f, "zeropage indexed by y"),
            AddressingMode::Indirect => write!(f, "indirect"),
            AddressingMode::IndirectX => write!(f, "indirect indexed by x"),
            AddressingMode::IndirectY => write!(f, "indirect indexed by y"),
        }
    }
}

/// All the regular instructions that the NES/Famicom can actually run.
#[derive(Clone, Debug)]
pub enum InstructionIdentifier {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Bit,
    Beq,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rti,
    Rts,
    Rol,
    Ror,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
}

/// An entry to the 'INSTRUCTIONS' map, which holds some values relevant for
/// understanding the associated instruction.
#[derive(Debug)]
pub struct ShortEntry {
    pub cycles: u8,
    pub opcode: u8,
    pub size: u8,
    pub affected_on_page: bool,
}

/// An Instruction from the Ricoh 2A03 chip. This is mainly used in the
/// 'OPCODES' map, and it's an expansion of the 'ShortEntry' struct.
#[derive(Clone, Debug)]
pub struct Instruction {
    pub identifier: InstructionIdentifier,
    pub addressing_mode: AddressingMode,
    pub cycles: u8,
    pub opcode: u8,
    pub size: u8,
    pub affected_on_page: bool,
    pub bytes: [u8; 2],
}

impl Instruction {
    /// Returns the value that can be obtained by evaluating the 'bytes' member
    /// as a little endian decimal.
    pub fn value(&self) -> usize {
        match self.size {
            2 => self.bytes[0] as usize,
            3 => self.bytes[0] as usize + ((self.bytes[1] as usize) << 8),
            _ => 0,
        }
    }

    /// Returns true if this is a branching instruction.
    pub fn is_branch(&self) -> bool {
        matches!(
            self.identifier,
            InstructionIdentifier::Bcc
                | InstructionIdentifier::Bcs
                | InstructionIdentifier::Beq
                | InstructionIdentifier::Bmi
                | InstructionIdentifier::Bne
                | InstructionIdentifier::Bpl
                | InstructionIdentifier::Bvc
                | InstructionIdentifier::Bvs
        )
    }

    /// Given the 'current' target address, and the helper 'memories' and
    /// 'addresses' maps, attempt to return a String representation of this
    /// instruction which is as human-readable as possible. This involves trying
    /// to resolve addresses/values from either maps.
    pub fn to_human(
        &self,
        current: usize,
        filter: Option<&str>,
        memories: &HashMap<usize, String>,
        addresses: &HashMap<usize, String>,
    ) -> String {
        let id = match self.identifier {
            InstructionIdentifier::Adc => "adc",
            InstructionIdentifier::And => "and",
            InstructionIdentifier::Asl => "asl",
            InstructionIdentifier::Bcc => "bcc",
            InstructionIdentifier::Bcs => "bcs",
            InstructionIdentifier::Bit => "bit",
            InstructionIdentifier::Beq => "beq",
            InstructionIdentifier::Bne => "bne",
            InstructionIdentifier::Bpl => "bpl",
            InstructionIdentifier::Bmi => "bmi",
            InstructionIdentifier::Brk => "brk",
            InstructionIdentifier::Bvc => "bvc",
            InstructionIdentifier::Bvs => "bvs",
            InstructionIdentifier::Clc => "clc",
            InstructionIdentifier::Cld => "cld",
            InstructionIdentifier::Cli => "cli",
            InstructionIdentifier::Clv => "clv",
            InstructionIdentifier::Cmp => "cmp",
            InstructionIdentifier::Cpx => "cpx",
            InstructionIdentifier::Cpy => "cpy",
            InstructionIdentifier::Dec => "dec",
            InstructionIdentifier::Dex => "dex",
            InstructionIdentifier::Dey => "dey",
            InstructionIdentifier::Eor => "eor",
            InstructionIdentifier::Inc => "inc",
            InstructionIdentifier::Inx => "inx",
            InstructionIdentifier::Iny => "iny",
            InstructionIdentifier::Jmp => "jmp",
            InstructionIdentifier::Jsr => "jsr",
            InstructionIdentifier::Lda => "lda",
            InstructionIdentifier::Ldx => "ldx",
            InstructionIdentifier::Ldy => "ldy",
            InstructionIdentifier::Lsr => "lsr",
            InstructionIdentifier::Nop => "nop",
            InstructionIdentifier::Ora => "ora",
            InstructionIdentifier::Pha => "pha",
            InstructionIdentifier::Php => "php",
            InstructionIdentifier::Pla => "pla",
            InstructionIdentifier::Plp => "plp",
            InstructionIdentifier::Rol => "rol",
            InstructionIdentifier::Ror => "ror",
            InstructionIdentifier::Rti => "rti",
            InstructionIdentifier::Rts => "rts",
            InstructionIdentifier::Sbc => "sbc",
            InstructionIdentifier::Sec => "sec",
            InstructionIdentifier::Sed => "sed",
            InstructionIdentifier::Sei => "sei",
            InstructionIdentifier::Sta => "sta",
            InstructionIdentifier::Stx => "stx",
            InstructionIdentifier::Sty => "sty",
            InstructionIdentifier::Tax => "tax",
            InstructionIdentifier::Tay => "tay",
            InstructionIdentifier::Tsx => "tsx",
            InstructionIdentifier::Txa => "txa",
            InstructionIdentifier::Txs => "txs",
            InstructionIdentifier::Tya => "tya",
        };

        // On most addressing modes, if the current value is actually found on
        // the map of known reserved memory addresses, then we can blindly try
        // to fetch it first. For relative, zeropage and absolute things are
        // more nuanced, so we delay this check in these scenarios.
        if matches!(
            self.addressing_mode,
            AddressingMode::IndexedX
                | AddressingMode::IndexedY
                | AddressingMode::ZeropageIndexedX
                | AddressingMode::ZeropageIndexedY
                | AddressingMode::Indirect
                | AddressingMode::IndirectX
                | AddressingMode::IndirectY
        ) {
            let val = self.value();
            if let Some(name) = memories.get(&val) {
                return format!("{id} {name}");
            }
        }

        match self.addressing_mode {
            AddressingMode::Immediate => {
                let val = self.value();
                if val > u8::MAX as usize {
                    format!("{id} #${:04X}", val)
                } else {
                    format!("{id} #${:02X}", val)
                }
            }
            AddressingMode::RelativeOrZeropage | AddressingMode::Absolute => {
                let val = self.value();

                // If this is a branch instruction, then we compute the final
                // address via the two's complement encoding of the given byte
                // plus the current address + 2 bytes to account for the branch
                // instruction itself. If all of that gives us a known address,
                // then use it, otherwise treat it as a relative/zeropage
                // nomenclature.
                if self.is_branch() {
                    let target = current as isize + (val as i8) as isize + 2;

                    if let Some(name) = addresses.get(&(target as usize)) {
                        let clean_name = match filter {
                            Some(f) => name.strip_prefix(f).unwrap_or(name),
                            None => name,
                        };
                        return format!("{id} {clean_name}");
                    }
                    return format!("{id} ${:02X}", val);
                }

                // There's no relative address to compute, so we check whether
                // this is a known absolute address.
                if let Some(name) = addresses.get(&val) {
                    // let last_name = name.split("::").last().unwrap_or(name);
                    // return format!("{id} {name}");
                    let clean_name = match filter {
                        Some(f) => name.strip_prefix(f).unwrap_or(name),
                        None => name,
                    };
                    return format!("{id} {clean_name}");
                }

                // Is it on the map ok known memory addresses?
                let val = self.value();
                if let Some(name) = memories.get(&val) {
                    return format!("{id} {name}");
                }

                if val > u8::MAX as usize {
                    format!("{id} ${:04X}", val)
                } else {
                    format!("{id} ${:02X}", val)
                }
            }
            AddressingMode::IndexedX => format!("{} ${:04X}, x", id, self.value()),
            AddressingMode::IndexedY => format!("{} ${:04X}, y", id, self.value()),
            AddressingMode::ZeropageIndexedX => format!("{} ${:02X}, x", id, self.value()),
            AddressingMode::ZeropageIndexedY => format!("{} ${:02X}, y", id, self.value()),
            AddressingMode::Indirect => format!("{} (${:02X})", id, self.value()),
            AddressingMode::IndirectX => format!("{} (${:02X}, x)", id, self.value()),
            AddressingMode::IndirectY => format!("{} (${:02X}), y", id, self.value()),
            AddressingMode::Implied => id.to_string(),
        }
    }
}

/// The representation of a Control statement/expression.
#[derive(Debug)]
pub struct Control {
    /// Type of control statement/expression.
    pub control_type: ControlType,

    /// Whether there is an expected identifier here, either by the parser or
    /// from an assembler perspective (e.g. 'Foo' in '.proc Foo'). A None value
    /// means that there is no identifier expected, Some(true) means that the
    /// identifier is not real but has to be created on the fly by the parser
    /// (e.g. .repeat).
    pub has_identifier: Option<bool>,

    /// Whether the control statement only has one mandatory argument which is a
    /// double-quoted string. If set to true, then `required_args` is ignored in
    /// favor of this.
    pub only_string: bool,

    /// Minimum and maximum number of arguments accepted by this control
    /// statement, or None if undefined (e.g. a .macro which has an undefined
    /// number of arguments).
    pub required_args: Option<(usize, usize)>,

    /// True if the context has to change after evaluating this control
    /// statement.
    pub touches_context: bool,
}

/// Map of insturctions known to this assembler. They can be accessed by passing
/// first the identifier (e.g. "lda"), and then the `AddressingMode`.
pub static INSTRUCTIONS: LazyLock<HashMap<String, HashMap<AddressingMode, ShortEntry>>> =
    LazyLock::new(|| {
        let mut instrs = HashMap::new();

        // adc
        let mut adc = HashMap::new();
        adc.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x69,
                affected_on_page: false,
            },
        );
        adc.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x65,
                affected_on_page: false,
            },
        );
        adc.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x75,
                affected_on_page: false,
            },
        );
        adc.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x6D,
                affected_on_page: false,
            },
        );
        adc.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x7D,
                affected_on_page: true,
            },
        );
        adc.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x79,
                affected_on_page: true,
            },
        );
        adc.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x61,
                affected_on_page: false,
            },
        );
        adc.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x71,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("adc"), adc);

        // and
        let mut and = HashMap::new();
        and.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x29,
                affected_on_page: false,
            },
        );
        and.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x25,
                affected_on_page: false,
            },
        );
        and.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x35,
                affected_on_page: false,
            },
        );
        and.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x2D,
                affected_on_page: false,
            },
        );
        and.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x3D,
                affected_on_page: true,
            },
        );
        and.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x39,
                affected_on_page: true,
            },
        );
        and.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x21,
                affected_on_page: false,
            },
        );
        and.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x31,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("and"), and);

        // asl
        let mut asl = HashMap::new();
        asl.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x0A,
                affected_on_page: false,
            },
        );
        asl.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x06,
                affected_on_page: false,
            },
        );
        asl.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x16,
                affected_on_page: false,
            },
        );
        asl.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0x0E,
                affected_on_page: false,
            },
        );
        asl.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0x1E,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("asl"), asl);

        // bcc
        let mut bcc = HashMap::new();
        bcc.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x90,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bcc"), bcc);

        // bcs
        let mut bcs = HashMap::new();
        bcs.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xB0,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bcs"), bcs);

        // beq
        let mut beq = HashMap::new();
        beq.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xF0,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("beq"), beq);

        // bit
        let mut bit = HashMap::new();
        bit.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x24,
                affected_on_page: false,
            },
        );
        bit.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x2C,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("bit"), bit);

        // bmi
        let mut bmi = HashMap::new();
        bmi.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x30,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bmi"), bmi);

        // bne
        let mut bne = HashMap::new();
        bne.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xD0,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bne"), bne);

        // bpl
        let mut bpl = HashMap::new();
        bpl.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x10,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bpl"), bpl);

        // brk
        let mut brk = HashMap::new();
        brk.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 7,
                size: 1,
                opcode: 0x00,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("brk"), brk);

        // bvc
        let mut bvc = HashMap::new();
        bvc.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x50,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bvc"), bvc);

        // bvs
        let mut bvs = HashMap::new();
        bvs.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x70,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("bvs"), bvs);

        // clc
        let mut clc = HashMap::new();
        clc.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x18,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("clc"), clc);

        // cld
        let mut cld = HashMap::new();
        cld.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xD8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("cld"), cld);

        // cli
        let mut cli = HashMap::new();
        cli.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x58,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("cli"), cli);

        // clv
        let mut clv = HashMap::new();
        clv.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xB8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("clv"), clv);

        // cmp
        let mut cmp = HashMap::new();
        cmp.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xC9,
                affected_on_page: false,
            },
        );
        cmp.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xC5,
                affected_on_page: false,
            },
        );
        cmp.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0xD5,
                affected_on_page: false,
            },
        );
        cmp.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xCD,
                affected_on_page: false,
            },
        );
        cmp.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xDD,
                affected_on_page: true,
            },
        );
        cmp.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xD9,
                affected_on_page: true,
            },
        );
        cmp.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0xC1,
                affected_on_page: false,
            },
        );
        cmp.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0xD1,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("cmp"), cmp);

        // cpx
        let mut cpx = HashMap::new();
        cpx.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xE0,
                affected_on_page: false,
            },
        );
        cpx.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xE4,
                affected_on_page: false,
            },
        );
        cpx.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xEC,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("cpx"), cpx);

        // cpy
        let mut cpy = HashMap::new();
        cpy.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xC0,
                affected_on_page: false,
            },
        );
        cpy.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xC4,
                affected_on_page: false,
            },
        );
        cpy.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xCC,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("cpy"), cpy);

        // dec
        let mut dec = HashMap::new();
        dec.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0xC6,
                affected_on_page: false,
            },
        );
        dec.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0xD6,
                affected_on_page: false,
            },
        );
        dec.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0xCE,
                affected_on_page: false,
            },
        );
        dec.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0xDE,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("dec"), dec);

        // dex
        let mut dex = HashMap::new();
        dex.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xCA,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("dex"), dex);

        // dey
        let mut dey = HashMap::new();
        dey.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x88,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("dey"), dey);

        // eor
        let mut eor = HashMap::new();
        eor.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x49,
                affected_on_page: false,
            },
        );
        eor.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x45,
                affected_on_page: false,
            },
        );
        eor.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x55,
                affected_on_page: false,
            },
        );
        eor.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x4D,
                affected_on_page: false,
            },
        );
        eor.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x5D,
                affected_on_page: true,
            },
        );
        eor.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x59,
                affected_on_page: true,
            },
        );
        eor.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x41,
                affected_on_page: false,
            },
        );
        eor.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x51,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("eor"), eor);

        // inc
        let mut inc = HashMap::new();
        inc.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0xE6,
                affected_on_page: false,
            },
        );
        inc.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0xF6,
                affected_on_page: false,
            },
        );
        inc.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0xEE,
                affected_on_page: false,
            },
        );
        inc.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0xFE,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("inc"), inc);

        // inx
        let mut inx = HashMap::new();
        inx.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xE8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("inx"), inx);

        // iny
        let mut iny = HashMap::new();
        iny.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xC8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("iny"), iny);

        // jmp
        let mut jmp = HashMap::new();
        jmp.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 3,
                size: 3,
                opcode: 0x4C,
                affected_on_page: false,
            },
        );
        jmp.insert(
            AddressingMode::Indirect,
            ShortEntry {
                cycles: 5,
                size: 3,
                opcode: 0x6C,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("jmp"), jmp);

        // jsr
        let mut jsr = HashMap::new();
        jsr.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0x20,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("jsr"), jsr);

        // lda
        let mut lda = HashMap::new();
        lda.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xA9,
                affected_on_page: false,
            },
        );
        lda.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xA5,
                affected_on_page: false,
            },
        );
        lda.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0xB5,
                affected_on_page: false,
            },
        );
        lda.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xAD,
                affected_on_page: false,
            },
        );
        lda.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xBD,
                affected_on_page: true,
            },
        );
        lda.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xB9,
                affected_on_page: true,
            },
        );
        lda.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0xA1,
                affected_on_page: false,
            },
        );
        lda.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0xB1,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("lda"), lda);

        // ldx
        let mut ldx = HashMap::new();
        ldx.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xA2,
                affected_on_page: false,
            },
        );
        ldx.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xA6,
                affected_on_page: false,
            },
        );
        ldx.insert(
            AddressingMode::ZeropageIndexedY,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0xB6,
                affected_on_page: false,
            },
        );
        ldx.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xAE,
                affected_on_page: false,
            },
        );
        ldx.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xBE,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("ldx"), ldx);

        // ldy
        let mut ldy = HashMap::new();
        ldy.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xA0,
                affected_on_page: false,
            },
        );
        ldy.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xA4,
                affected_on_page: false,
            },
        );
        ldy.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0xB4,
                affected_on_page: false,
            },
        );
        ldy.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xAC,
                affected_on_page: false,
            },
        );
        ldy.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xBC,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("ldy"), ldy);

        // lsr
        let mut lsr = HashMap::new();
        lsr.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x4A,
                affected_on_page: false,
            },
        );
        lsr.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x46,
                affected_on_page: false,
            },
        );
        lsr.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x56,
                affected_on_page: false,
            },
        );
        lsr.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0x4E,
                affected_on_page: false,
            },
        );
        lsr.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0x5E,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("lsr"), lsr);

        // nop
        let mut nop = HashMap::new();
        nop.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xEA,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("nop"), nop);

        // ora
        let mut ora = HashMap::new();
        ora.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0x09,
                affected_on_page: false,
            },
        );
        ora.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x05,
                affected_on_page: false,
            },
        );
        ora.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x15,
                affected_on_page: false,
            },
        );
        ora.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x0D,
                affected_on_page: false,
            },
        );
        ora.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x1D,
                affected_on_page: true,
            },
        );
        ora.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x19,
                affected_on_page: true,
            },
        );
        ora.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x01,
                affected_on_page: false,
            },
        );
        ora.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x11,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("ora"), ora);

        // pha
        let mut pha = HashMap::new();
        pha.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 3,
                size: 1,
                opcode: 0x48,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("pha"), pha);

        // php
        let mut php = HashMap::new();
        php.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 3,
                size: 1,
                opcode: 0x08,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("php"), php);

        // pla
        let mut pla = HashMap::new();
        pla.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 4,
                size: 1,
                opcode: 0x68,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("pla"), pla);

        // plp
        let mut plp = HashMap::new();
        plp.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 4,
                size: 1,
                opcode: 0x28,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("plp"), plp);

        // rol
        let mut rol = HashMap::new();
        rol.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x2A,
                affected_on_page: false,
            },
        );
        rol.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x26,
                affected_on_page: false,
            },
        );
        rol.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x36,
                affected_on_page: false,
            },
        );
        rol.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0x2E,
                affected_on_page: false,
            },
        );
        rol.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0x3E,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("rol"), rol);

        // ror
        let mut ror = HashMap::new();
        ror.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x6A,
                affected_on_page: false,
            },
        );
        ror.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0x66,
                affected_on_page: false,
            },
        );
        ror.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x76,
                affected_on_page: false,
            },
        );
        ror.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 6,
                size: 3,
                opcode: 0x6E,
                affected_on_page: false,
            },
        );
        ror.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 7,
                size: 3,
                opcode: 0x7E,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("ror"), ror);

        // rti
        let mut rti = HashMap::new();
        rti.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 6,
                size: 1,
                opcode: 0x40,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("rti"), rti);

        // rts
        let mut rts = HashMap::new();
        rts.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 6,
                size: 1,
                opcode: 0x60,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("rts"), rts);

        // sbc
        let mut sbc = HashMap::new();
        sbc.insert(
            AddressingMode::Immediate,
            ShortEntry {
                cycles: 2,
                size: 2,
                opcode: 0xE9,
                affected_on_page: false,
            },
        );
        sbc.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0xE5,
                affected_on_page: false,
            },
        );
        sbc.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0xF5,
                affected_on_page: false,
            },
        );
        sbc.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xED,
                affected_on_page: false,
            },
        );
        sbc.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xFD,
                affected_on_page: true,
            },
        );
        sbc.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0xF9,
                affected_on_page: true,
            },
        );
        sbc.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0xE1,
                affected_on_page: false,
            },
        );
        sbc.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 5,
                size: 2,
                opcode: 0xF1,
                affected_on_page: true,
            },
        );
        instrs.insert(String::from("sbc"), sbc);

        // sec
        let mut sec = HashMap::new();
        sec.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x38,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("sec"), sec);

        // sed
        let mut sed = HashMap::new();
        sed.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xF8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("sed"), sed);

        // sei
        let mut sei = HashMap::new();
        sei.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x78,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("sei"), sei);

        // sta
        let mut sta = HashMap::new();
        sta.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x85,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x95,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x8D,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::IndexedX,
            ShortEntry {
                cycles: 5,
                size: 3,
                opcode: 0x9D,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::IndexedY,
            ShortEntry {
                cycles: 5,
                size: 3,
                opcode: 0x99,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::IndirectX,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x81,
                affected_on_page: false,
            },
        );
        sta.insert(
            AddressingMode::IndirectY,
            ShortEntry {
                cycles: 6,
                size: 2,
                opcode: 0x91,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("sta"), sta);

        // stx
        let mut stx = HashMap::new();
        stx.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x86,
                affected_on_page: false,
            },
        );
        stx.insert(
            AddressingMode::ZeropageIndexedY,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x96,
                affected_on_page: false,
            },
        );
        stx.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x8E,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("stx"), stx);

        // sty
        let mut sty = HashMap::new();
        sty.insert(
            AddressingMode::RelativeOrZeropage,
            ShortEntry {
                cycles: 3,
                size: 2,
                opcode: 0x84,
                affected_on_page: false,
            },
        );
        sty.insert(
            AddressingMode::ZeropageIndexedX,
            ShortEntry {
                cycles: 4,
                size: 2,
                opcode: 0x94,
                affected_on_page: false,
            },
        );
        sty.insert(
            AddressingMode::Absolute,
            ShortEntry {
                cycles: 4,
                size: 3,
                opcode: 0x8C,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("sty"), sty);

        // tax
        let mut tax = HashMap::new();
        tax.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xAA,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("tax"), tax);

        // tay
        let mut tay = HashMap::new();
        tay.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xA8,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("tay"), tay);

        // tsx
        let mut tsx = HashMap::new();
        tsx.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0xBA,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("tsx"), tsx);

        // txa
        let mut txa = HashMap::new();
        txa.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x8A,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("txa"), txa);

        // txs
        let mut txs = HashMap::new();
        txs.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x9A,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("txs"), txs);

        // tya
        let mut tya = HashMap::new();
        tya.insert(
            AddressingMode::Implied,
            ShortEntry {
                cycles: 2,
                size: 1,
                opcode: 0x98,
                affected_on_page: false,
            },
        );
        instrs.insert(String::from("tya"), tya);

        instrs
    });

/// Map of control functions known to this assembler.
pub static CONTROL_FUNCTIONS: LazyLock<HashMap<String, Control>> = LazyLock::new(|| {
    let mut functions = HashMap::new();

    functions.insert(
        String::from(".version"),
        Control {
            control_type: ControlType::Version,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".hibyte"),
        Control {
            control_type: ControlType::Hibyte,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".lobyte"),
        Control {
            control_type: ControlType::Lobyte,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".macro"),
        Control {
            control_type: ControlType::StartMacro,
            has_identifier: Some(false),
            required_args: None,
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".proc"),
        Control {
            control_type: ControlType::StartProc,
            has_identifier: Some(false),
            required_args: Some((0, 0)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".scope"),
        Control {
            control_type: ControlType::StartScope,
            has_identifier: Some(false),
            required_args: Some((0, 0)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".endscope"),
        Control {
            control_type: ControlType::EndScope,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".endproc"),
        Control {
            control_type: ControlType::EndProc,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".endmacro"),
        Control {
            control_type: ControlType::EndMacro,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".segment"),
        Control {
            control_type: ControlType::Segment,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".byte"),
        Control {
            control_type: ControlType::Byte,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".byt"),
        Control {
            control_type: ControlType::Byte,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".db"),
        Control {
            control_type: ControlType::Byte,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".word"),
        Control {
            control_type: ControlType::Word,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".dw"),
        Control {
            control_type: ControlType::Word,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".addr"),
        Control {
            control_type: ControlType::Addr,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".dbyt"),
        Control {
            control_type: ControlType::BigEndianWord,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".be"),
        Control {
            control_type: ControlType::BigEndianWord,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".bigendian"),
        Control {
            control_type: ControlType::BigEndianWord,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".max"),
        Control {
            control_type: ControlType::Max,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".min"),
        Control {
            control_type: ControlType::Min,
            has_identifier: None,
            required_args: None,
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".incbin"),
        Control {
            control_type: ControlType::IncBin,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".repeat"),
        Control {
            control_type: ControlType::StartRepeat,
            has_identifier: Some(true),
            required_args: Some((1, 2)),
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".endrepeat"),
        Control {
            control_type: ControlType::EndRepeat,
            has_identifier: None,
            required_args: None,
            touches_context: true,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".include"),
        Control {
            control_type: ControlType::IncludeSource,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".res"),
        Control {
            control_type: ControlType::ReserveMemory,
            has_identifier: None,
            required_args: Some((1, 2)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".asciiz"),
        Control {
            control_type: ControlType::Asciiz,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".if"),
        Control {
            control_type: ControlType::If,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".ifdef"),
        Control {
            control_type: ControlType::IfDef,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".ifndef"),
        Control {
            control_type: ControlType::IfNDef,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".elsif"),
        Control {
            control_type: ControlType::Elsif,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".else"),
        Control {
            control_type: ControlType::Else,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".endif"),
        Control {
            control_type: ControlType::EndIf,
            has_identifier: None,
            required_args: Some((0, 0)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".def"),
        Control {
            control_type: ControlType::Defined,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".defined"),
        Control {
            control_type: ControlType::Defined,
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: false,
        },
    );
    functions.insert(
        String::from(".info"),
        Control {
            control_type: ControlType::Echo(EchoKind::Info),
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".out"),
        Control {
            control_type: ControlType::Echo(EchoKind::Info),
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".warning"),
        Control {
            control_type: ControlType::Echo(EchoKind::Warning),
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );
    functions.insert(
        String::from(".error"),
        Control {
            control_type: ControlType::Echo(EchoKind::Error),
            has_identifier: None,
            required_args: Some((1, 1)),
            touches_context: false,
            only_string: true,
        },
    );

    functions
});

/// For a given byte opcode, return the Instruction associated with it.
pub static OPCODES: LazyLock<HashMap<u8, Instruction>> = LazyLock::new(|| {
    let mut ops = HashMap::new();

    // adc
    ops.insert(
        0x69,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0x69,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x65,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x65,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x75,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x75,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x6D,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x6D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x7D,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0x7D,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x79,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0x79,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x61,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0x61,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x71,
        Instruction {
            identifier: InstructionIdentifier::Adc,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0x71,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // and
    ops.insert(
        0x29,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0x29,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x25,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x25,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x35,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x35,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x2D,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x2D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x3D,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0x3D,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x39,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0x39,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x21,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0x21,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x31,
        Instruction {
            identifier: InstructionIdentifier::And,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0x31,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // asl
    ops.insert(
        0x0A,
        Instruction {
            identifier: InstructionIdentifier::Asl,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x0A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x06,
        Instruction {
            identifier: InstructionIdentifier::Asl,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0x06,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x16,
        Instruction {
            identifier: InstructionIdentifier::Asl,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0x16,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x0E,
        Instruction {
            identifier: InstructionIdentifier::Asl,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0x0E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    ops.insert(
        0x1E,
        Instruction {
            identifier: InstructionIdentifier::Asl,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0x1E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // bit
    ops.insert(
        0x24,
        Instruction {
            identifier: InstructionIdentifier::Bit,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x24,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x2C,
        Instruction {
            identifier: InstructionIdentifier::Bit,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x2C,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // bcc
    ops.insert(
        0x90,
        Instruction {
            identifier: InstructionIdentifier::Bcc,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0x90,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // bcs
    ops.insert(
        0xB0,
        Instruction {
            identifier: InstructionIdentifier::Bcs,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0xB0,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // beq
    ops.insert(
        0xF0,
        Instruction {
            identifier: InstructionIdentifier::Beq,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0xF0,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // bne
    ops.insert(
        0xD0,
        Instruction {
            identifier: InstructionIdentifier::Bne,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0xD0,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // bpl
    ops.insert(
        0x10,
        Instruction {
            identifier: InstructionIdentifier::Bpl,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0x10,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // bmi
    ops.insert(
        0x30,
        Instruction {
            identifier: InstructionIdentifier::Bmi,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0x30,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // brk
    ops.insert(
        0x00,
        Instruction {
            identifier: InstructionIdentifier::Brk,
            addressing_mode: AddressingMode::Implied,
            cycles: 7,
            size: 1,
            opcode: 0x00,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // bvs
    ops.insert(
        0x70,
        Instruction {
            identifier: InstructionIdentifier::Bvs,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0x70,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // bvc
    ops.insert(
        0x50,
        Instruction {
            identifier: InstructionIdentifier::Bvc,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 2,
            opcode: 0x50,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // clc
    ops.insert(
        0x18,
        Instruction {
            identifier: InstructionIdentifier::Clc,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x18,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // cld
    ops.insert(
        0xD8,
        Instruction {
            identifier: InstructionIdentifier::Cld,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xD8,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // cli
    ops.insert(
        0x58,
        Instruction {
            identifier: InstructionIdentifier::Cli,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x58,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // clv
    ops.insert(
        0xB8,
        Instruction {
            identifier: InstructionIdentifier::Clv,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xB8,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // cmp
    ops.insert(
        0xC9,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xC9,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xC5,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xC5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xD5,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0xD5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xCD,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xCD,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xDD,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0xDD,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xD9,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0xD9,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xC1,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0xC1,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xD1,
        Instruction {
            identifier: InstructionIdentifier::Cmp,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0xD1,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // cpx
    ops.insert(
        0xE0,
        Instruction {
            identifier: InstructionIdentifier::Cpx,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xE0,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xE4,
        Instruction {
            identifier: InstructionIdentifier::Cpx,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xE4,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xEC,
        Instruction {
            identifier: InstructionIdentifier::Cpx,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xEC,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // cpy
    ops.insert(
        0xC0,
        Instruction {
            identifier: InstructionIdentifier::Cpy,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xC0,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xC4,
        Instruction {
            identifier: InstructionIdentifier::Cpy,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xC4,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xCC,
        Instruction {
            identifier: InstructionIdentifier::Cpy,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xCC,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // dec
    ops.insert(
        0xC6,
        Instruction {
            identifier: InstructionIdentifier::Dec,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0xC6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xD6,
        Instruction {
            identifier: InstructionIdentifier::Dec,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0xD6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xCE,
        Instruction {
            identifier: InstructionIdentifier::Dec,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0xCE,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xDE,
        Instruction {
            identifier: InstructionIdentifier::Dec,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0xDE,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // dex
    ops.insert(
        0xCA,
        Instruction {
            identifier: InstructionIdentifier::Dex,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xCA,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // dey
    ops.insert(
        0x88,
        Instruction {
            identifier: InstructionIdentifier::Dey,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x88,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // eor
    ops.insert(
        0x49,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0x49,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x45,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x45,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x55,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x55,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x4D,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x4D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x5D,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0x5D,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x59,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0x59,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x41,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0x41,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x51,
        Instruction {
            identifier: InstructionIdentifier::Eor,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0x51,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // inc
    ops.insert(
        0xE6,
        Instruction {
            identifier: InstructionIdentifier::Inc,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0xE6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xF6,
        Instruction {
            identifier: InstructionIdentifier::Inc,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0xF6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xEE,
        Instruction {
            identifier: InstructionIdentifier::Inc,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0xEE,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xFE,
        Instruction {
            identifier: InstructionIdentifier::Inc,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0xFE,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // inx
    ops.insert(
        0xE8,
        Instruction {
            identifier: InstructionIdentifier::Inx,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xE8,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // iny
    ops.insert(
        0xC8,
        Instruction {
            identifier: InstructionIdentifier::Iny,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xC8,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // jmp
    ops.insert(
        0x4C,
        Instruction {
            identifier: InstructionIdentifier::Jmp,
            addressing_mode: AddressingMode::Absolute,
            cycles: 3,
            opcode: 0x4C,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // jsr
    ops.insert(
        0x20,
        Instruction {
            identifier: InstructionIdentifier::Jsr,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0x20,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // lda
    ops.insert(
        0xA9,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xA9,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xA5,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xA5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xB5,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0xB5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xAD,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xAD,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xBD,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0xBD,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xB9,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0xB9,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xA1,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0xA1,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xB1,
        Instruction {
            identifier: InstructionIdentifier::Lda,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0xB1,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // ldx
    ops.insert(
        0xA2,
        Instruction {
            identifier: InstructionIdentifier::Ldx,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xA2,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xA6,
        Instruction {
            identifier: InstructionIdentifier::Ldx,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xA6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xB6,
        Instruction {
            identifier: InstructionIdentifier::Ldx,
            addressing_mode: AddressingMode::ZeropageIndexedY,
            cycles: 4,
            opcode: 0xB6,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xAE,
        Instruction {
            identifier: InstructionIdentifier::Ldx,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xAE,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xBE,
        Instruction {
            identifier: InstructionIdentifier::Ldx,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0xBE,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // ldy
    ops.insert(
        0xA0,
        Instruction {
            identifier: InstructionIdentifier::Ldy,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xA0,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xA4,
        Instruction {
            identifier: InstructionIdentifier::Ldy,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xA4,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xB4,
        Instruction {
            identifier: InstructionIdentifier::Ldy,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0xB4,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xAC,
        Instruction {
            identifier: InstructionIdentifier::Ldy,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xAC,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xBC,
        Instruction {
            identifier: InstructionIdentifier::Ldy,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0xBC,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // lsr
    ops.insert(
        0x4A,
        Instruction {
            identifier: InstructionIdentifier::Lsr,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x4A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x46,
        Instruction {
            identifier: InstructionIdentifier::Lsr,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0x46,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x56,
        Instruction {
            identifier: InstructionIdentifier::Lsr,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0x56,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x4E,
        Instruction {
            identifier: InstructionIdentifier::Lsr,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0x4E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    ops.insert(
        0x5E,
        Instruction {
            identifier: InstructionIdentifier::Lsr,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0x5E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // nop
    ops.insert(
        0xEA,
        Instruction {
            identifier: InstructionIdentifier::Nop,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xEA,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // ora
    ops.insert(
        0x09,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0x09,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x05,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x05,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x15,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x15,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x0D,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x0D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x1D,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0x1D,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x19,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0x19,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x01,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0x01,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x11,
        Instruction {
            identifier: InstructionIdentifier::Ora,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0x11,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // pha
    ops.insert(
        0x48,
        Instruction {
            identifier: InstructionIdentifier::Pha,
            addressing_mode: AddressingMode::Implied,
            cycles: 3,
            size: 1,
            opcode: 0x48,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // php
    ops.insert(
        0x08,
        Instruction {
            identifier: InstructionIdentifier::Php,
            addressing_mode: AddressingMode::Implied,
            cycles: 3,
            size: 1,
            opcode: 0x08,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // pla
    ops.insert(
        0x68,
        Instruction {
            identifier: InstructionIdentifier::Pla,
            addressing_mode: AddressingMode::Implied,
            cycles: 3,
            size: 1,
            opcode: 0x68,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // plp
    ops.insert(
        0x28,
        Instruction {
            identifier: InstructionIdentifier::Plp,
            addressing_mode: AddressingMode::Implied,
            cycles: 3,
            size: 1,
            opcode: 0x28,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // rol
    ops.insert(
        0x2A,
        Instruction {
            identifier: InstructionIdentifier::Rol,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x2A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x26,
        Instruction {
            identifier: InstructionIdentifier::Rol,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0x26,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x36,
        Instruction {
            identifier: InstructionIdentifier::Rol,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0x36,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x2E,
        Instruction {
            identifier: InstructionIdentifier::Rol,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0x2E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x3E,
        Instruction {
            identifier: InstructionIdentifier::Rol,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0x3E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // ror
    ops.insert(
        0x6A,
        Instruction {
            identifier: InstructionIdentifier::Ror,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x6A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x66,
        Instruction {
            identifier: InstructionIdentifier::Ror,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 5,
            opcode: 0x66,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x76,
        Instruction {
            identifier: InstructionIdentifier::Ror,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 6,
            opcode: 0x76,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x6E,
        Instruction {
            identifier: InstructionIdentifier::Ror,
            addressing_mode: AddressingMode::Absolute,
            cycles: 6,
            opcode: 0x6E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x7E,
        Instruction {
            identifier: InstructionIdentifier::Ror,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 7,
            opcode: 0x7E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // rti
    ops.insert(
        0x40,
        Instruction {
            identifier: InstructionIdentifier::Rti,
            addressing_mode: AddressingMode::Implied,
            cycles: 6,
            opcode: 0x40,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // rts
    ops.insert(
        0x60,
        Instruction {
            identifier: InstructionIdentifier::Rts,
            addressing_mode: AddressingMode::Implied,
            cycles: 6,
            opcode: 0x60,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // sbc
    ops.insert(
        0xE9,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::Immediate,
            cycles: 2,
            opcode: 0xE9,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xE5,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0xE5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xF5,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0xF5,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xED,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0xED,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xFD,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 4,
            opcode: 0xFD,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xF9,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 4,
            opcode: 0xF9,
            size: 3,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xE1,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0xE1,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0xF1,
        Instruction {
            identifier: InstructionIdentifier::Sbc,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 5,
            opcode: 0xF1,
            size: 2,
            affected_on_page: true,
            bytes: [0, 0],
        },
    );

    // sec
    ops.insert(
        0x38,
        Instruction {
            identifier: InstructionIdentifier::Sec,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x38,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // sed
    ops.insert(
        0x38,
        Instruction {
            identifier: InstructionIdentifier::Sed,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            size: 1,
            opcode: 0x38,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // sei
    ops.insert(
        0x78,
        Instruction {
            identifier: InstructionIdentifier::Sei,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x78,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // sta
    ops.insert(
        0x85,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x85,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x95,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x95,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x8D,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x8D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x9D,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::IndexedX,
            cycles: 5,
            opcode: 0x9D,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x99,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::IndexedY,
            cycles: 5,
            opcode: 0x99,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x81,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::IndirectX,
            cycles: 6,
            opcode: 0x81,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x91,
        Instruction {
            identifier: InstructionIdentifier::Sta,
            addressing_mode: AddressingMode::IndirectY,
            cycles: 6,
            opcode: 0x91,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // stx
    ops.insert(
        0x86,
        Instruction {
            identifier: InstructionIdentifier::Stx,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x86,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x96,
        Instruction {
            identifier: InstructionIdentifier::Stx,
            addressing_mode: AddressingMode::ZeropageIndexedY,
            cycles: 4,
            opcode: 0x96,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x8E,
        Instruction {
            identifier: InstructionIdentifier::Stx,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x8E,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // sty
    ops.insert(
        0x84,
        Instruction {
            identifier: InstructionIdentifier::Sty,
            addressing_mode: AddressingMode::RelativeOrZeropage,
            cycles: 3,
            opcode: 0x84,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x94,
        Instruction {
            identifier: InstructionIdentifier::Sty,
            addressing_mode: AddressingMode::ZeropageIndexedX,
            cycles: 4,
            opcode: 0x94,
            size: 2,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );
    ops.insert(
        0x8C,
        Instruction {
            identifier: InstructionIdentifier::Sty,
            addressing_mode: AddressingMode::Absolute,
            cycles: 4,
            opcode: 0x8C,
            size: 3,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // tax
    ops.insert(
        0xAA,
        Instruction {
            identifier: InstructionIdentifier::Tax,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xAA,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // tay
    ops.insert(
        0xA8,
        Instruction {
            identifier: InstructionIdentifier::Tay,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xA8,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // tsx
    ops.insert(
        0xBA,
        Instruction {
            identifier: InstructionIdentifier::Tsx,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0xBA,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // txa
    ops.insert(
        0x8A,
        Instruction {
            identifier: InstructionIdentifier::Txa,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x8A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // txs
    ops.insert(
        0x9A,
        Instruction {
            identifier: InstructionIdentifier::Txs,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x9A,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    // tya
    ops.insert(
        0x98,
        Instruction {
            identifier: InstructionIdentifier::Tya,
            addressing_mode: AddressingMode::Implied,
            cycles: 2,
            opcode: 0x98,
            size: 1,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

    ops
});
