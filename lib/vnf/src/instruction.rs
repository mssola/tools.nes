use std::collections::HashMap;
use std::sync::LazyLock;
use xixanta::opcodes::AddressingMode;

#[derive(Clone, Debug)]
pub enum InstructionIdentifier {
    Unknown,
    Start,
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
    pub fn value(&self) -> usize {
        match self.size {
            2 => self.bytes[0] as usize,
            3 => self.bytes[0] as usize + ((self.bytes[1] as usize) << 8),
            _ => 0,
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
            InstructionIdentifier::Rol => "rol",
            InstructionIdentifier::Ror => "ror",
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
            InstructionIdentifier::Start => "<start>",
            InstructionIdentifier::Unknown => "<unknown>",
        };

        match self.addressing_mode {
            AddressingMode::Immediate => {
                let val = self.value();
                if val > u8::MAX as usize {
                    write!(f, "{} #${:04X}", id, val)
                } else {
                    write!(f, "{} #${:02X}", id, val)
                }
            }
            AddressingMode::RelativeOrZeropage | AddressingMode::Absolute => {
                let val = self.value();
                if val > u8::MAX as usize {
                    write!(f, "{} ${:04X}", id, val)
                } else {
                    write!(f, "{} ${:02X}", id, val)
                }
            }
            AddressingMode::IndexedX => write!(f, "{} ${:04X}, x", id, self.value()),
            AddressingMode::IndexedY => write!(f, "{} ${:04X}, y", id, self.value()),
            AddressingMode::ZeropageIndexedX => write!(f, "{} ${:02X}, x", id, self.value()),
            AddressingMode::ZeropageIndexedY => write!(f, "{} ${:02X}, y", id, self.value()),
            AddressingMode::Indirect => write!(f, "{} (${:02X})", id, self.value()),
            AddressingMode::IndirectX => write!(f, "{} (${:02X}, x)", id, self.value()),
            AddressingMode::IndirectY => write!(f, "{} (${:02X}), y", id, self.value()),
            AddressingMode::Implied => write!(f, "{}", id),
        }
    }
}

pub static OPCODES: LazyLock<HashMap<u8, Instruction>> = LazyLock::new(|| {
    let mut ops = HashMap::new();

    ops.insert(
        0x00,
        Instruction {
            identifier: InstructionIdentifier::Unknown,
            addressing_mode: AddressingMode::Implied,
            cycles: 0,
            opcode: 0x00,
            size: 0,
            affected_on_page: false,
            bytes: [0, 0],
        },
    );

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
