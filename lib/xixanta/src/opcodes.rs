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

#[derive(Debug)]
pub struct ShortEntry {
    pub cycles: u8,
    pub opcode: u8,
    pub size: u8,
    pub affected_on_page: bool,
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
    functions.insert(
        String::from(".fallthrough"),
        Control {
            control_type: ControlType::Fallthrough,
            has_identifier: Some(false),
            required_args: Some((0, 0)),
            touches_context: false,
            only_string: false,
        },
    );

    functions
});
