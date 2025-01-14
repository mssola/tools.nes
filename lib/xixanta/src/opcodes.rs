use crate::node::ControlType;
use std::collections::HashMap;
use std::fmt;

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

// #[derive(Debug)]
// pub struct Entry {
//     pub mode: AddressingMode,
//     pub mnemonic: String,
//     pub cycles: u8,
//     pub opcode: u8,
//     pub size: u8,
//     pub affected_on_page: bool,
// }

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

    /// Minimum and maximum number of arguments accepted by this control
    /// statement, or None if undefined (e.g. a .macro which has an undefined
    /// number of arguments).
    pub required_args: Option<(usize, usize)>,

    /// True if the context has to change after evaluating this control
    /// statement.
    pub touches_context: bool,
}

lazy_static! {
    pub static ref INSTRUCTIONS: HashMap<String, HashMap<AddressingMode, ShortEntry>> = {
        let mut instrs = HashMap::new();

        // adc
        let mut adc = HashMap::new();
        adc.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0x69, affected_on_page: false });
        adc.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x65, affected_on_page: false });
        adc.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x75, affected_on_page: false });
        adc.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x6D, affected_on_page: false });
        adc.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0x7D, affected_on_page: true });
        adc.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0x79, affected_on_page: true });
        adc.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0x61, affected_on_page: false });
        adc.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0x71, affected_on_page: true });
        instrs.insert(String::from("adc"), adc);

        // and
        let mut and = HashMap::new();
        and.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0x29, affected_on_page: false });
        and.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x25, affected_on_page: false });
        and.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x35, affected_on_page: false });
        and.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x2D, affected_on_page: false });
        and.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0x3D, affected_on_page: true });
        and.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0x39, affected_on_page: true });
        and.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0x21, affected_on_page: false });
        and.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0x31, affected_on_page: true });
        instrs.insert(String::from("and"), and);

        // asl
        let mut asl = HashMap::new();
        asl.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x0A, affected_on_page: false });
        asl.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 5, size: 2, opcode: 0x06, affected_on_page: false });
        asl.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 6, size: 2, opcode: 0x16, affected_on_page: false });
        asl.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0x0E, affected_on_page: false });
        asl.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 7, size: 3, opcode: 0x1E, affected_on_page: false });
        instrs.insert(String::from("asl"), asl);

        // bcc
        let mut bcc = HashMap::new();
        bcc.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0x90, affected_on_page: true });
        instrs.insert(String::from("bcc"), bcc);

        // bcs
        let mut bcs = HashMap::new();
        bcs.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0xB0, affected_on_page: true });
        instrs.insert(String::from("bcs"), bcs);

        // beq
        let mut beq = HashMap::new();
        beq.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0xF0, affected_on_page: true });
        instrs.insert(String::from("beq"), beq);

        // bit
        let mut bit = HashMap::new();
        bit.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x24, affected_on_page: false });
        bit.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x2C, affected_on_page: false });
        instrs.insert(String::from("bit"), bit);

        // bmi
        let mut bmi = HashMap::new();
        bmi.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0x30, affected_on_page: true });
        instrs.insert(String::from("bmi"), bmi);

        // bne
        let mut bne = HashMap::new();
        bne.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0xD0, affected_on_page: true });
        instrs.insert(String::from("bne"), bne);

        // bpl
        let mut bpl = HashMap::new();
        bpl.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0x10, affected_on_page: true });
        instrs.insert(String::from("bpl"), bpl);

        // brk
        let mut brk = HashMap::new();
        brk.insert(AddressingMode::Implied, ShortEntry{ cycles: 7, size: 1, opcode: 0x00, affected_on_page: false });
        instrs.insert(String::from("brk"), brk);

        // bvc
        let mut bvc = HashMap::new();
        bvc.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0x50, affected_on_page: true });
        instrs.insert(String::from("bvc"), bvc);

        // bvs
        let mut bvs = HashMap::new();
        bvs.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 2, size: 2, opcode: 0x70, affected_on_page: true });
        instrs.insert(String::from("bvs"), bvs);

        // clc
        let mut clc = HashMap::new();
        clc.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x18, affected_on_page: false });
        instrs.insert(String::from("clc"), clc);

        // cld
        let mut cld = HashMap::new();
        cld.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xD8, affected_on_page: false });
        instrs.insert(String::from("cld"), cld);

        // cli
        let mut cli = HashMap::new();
        cli.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x58, affected_on_page: false });
        instrs.insert(String::from("cli"), cli);

        // clv
        let mut clv = HashMap::new();
        clv.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xB8, affected_on_page: false });
        instrs.insert(String::from("clv"), clv);

        // cmp
        let mut cmp = HashMap::new();
        cmp.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xC9, affected_on_page: false });
        cmp.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xC5, affected_on_page: false });
        cmp.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0xD5, affected_on_page: false });
        cmp.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xCD, affected_on_page: false });
        cmp.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0xDD, affected_on_page: true });
        cmp.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0xD9, affected_on_page: true });
        cmp.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0xC1, affected_on_page: false });
        cmp.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0xD1, affected_on_page: true });
        instrs.insert(String::from("cmp"), cmp);

        // cpx
        let mut cpx = HashMap::new();
        cpx.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xE0, affected_on_page: false });
        cpx.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xE4, affected_on_page: false });
        cpx.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xEC, affected_on_page: false });
        instrs.insert(String::from("cpx"), cpx);

        // cpy
        let mut cpy = HashMap::new();
        cpy.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xC0, affected_on_page: false });
        cpy.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xC4, affected_on_page: false });
        cpy.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xCC, affected_on_page: false });
        instrs.insert(String::from("cpy"), cpy);

        // dec
        let mut dec = HashMap::new();
        dec.insert(AddressingMode::RelativeOrZeropage, ShortEntry { cycles: 5, size: 2, opcode: 0xC6, affected_on_page: false });
        dec.insert(AddressingMode::ZeropageIndexedX, ShortEntry { cycles: 6, size: 2, opcode: 0xD6, affected_on_page: false });
        dec.insert(AddressingMode::Absolute, ShortEntry { cycles: 6, size: 3, opcode: 0xCE, affected_on_page: false });
        dec.insert(AddressingMode::IndexedX, ShortEntry { cycles: 7, size: 3, opcode: 0xDE, affected_on_page: false });
        instrs.insert(String::from("dec"), dec);

        // dex
        let mut dex = HashMap::new();
        dex.insert(AddressingMode::Implied, ShortEntry { cycles: 2, size: 1, opcode: 0xCA, affected_on_page: false });
        instrs.insert(String::from("dex"), dex);

        // dey
        let mut dey = HashMap::new();
        dey.insert(AddressingMode::Implied, ShortEntry { cycles: 2, size: 1, opcode: 0x88, affected_on_page: false });
        instrs.insert(String::from("dey"), dey);

        // eor
        let mut eor = HashMap::new();
        eor.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0x49, affected_on_page: false });
        eor.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x45, affected_on_page: false });
        eor.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x55, affected_on_page: false });
        eor.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x4D, affected_on_page: false });
        eor.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0x5D, affected_on_page: true });
        eor.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0x59, affected_on_page: true });
        eor.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0x41, affected_on_page: false });
        eor.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0x51, affected_on_page: true });
        instrs.insert(String::from("eor"), eor);

        // inc
        let mut inc = HashMap::new();
        inc.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 5, size: 2, opcode: 0xE6, affected_on_page: false });
        inc.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 6, size: 2, opcode: 0xF6, affected_on_page: false });
        inc.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0xEE, affected_on_page: false });
        inc.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 7, size: 3, opcode: 0xFE, affected_on_page: false });
        instrs.insert(String::from("inc"), inc);

        // inx
        let mut inx = HashMap::new();
        inx.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xE8, affected_on_page: false });
        instrs.insert(String::from("inx"), inx);

        // iny
        let mut iny = HashMap::new();
        iny.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xC8, affected_on_page: false });
        instrs.insert(String::from("iny"), iny);

        // jmp
        let mut jmp = HashMap::new();
        jmp.insert(AddressingMode::Absolute, ShortEntry{ cycles: 3, size: 3, opcode: 0x4C, affected_on_page: false });
        jmp.insert(AddressingMode::Indirect, ShortEntry{ cycles: 5, size: 3, opcode: 0x6C, affected_on_page: false });
        instrs.insert(String::from("jmp"), jmp);

        // jsr
        let mut jsr = HashMap::new();
        jsr.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0x20, affected_on_page: false });
        instrs.insert(String::from("jsr"), jsr);

        // lda
        let mut lda = HashMap::new();
        lda.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xA9, affected_on_page: false });
        lda.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xA5, affected_on_page: false });
        lda.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0xB5, affected_on_page: false });
        lda.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xAD, affected_on_page: false });
        lda.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0xBD, affected_on_page: true });
        lda.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0xB9, affected_on_page: true });
        lda.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0xA1, affected_on_page: false });
        lda.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0xB1, affected_on_page: true });
        instrs.insert(String::from("lda"), lda);

        // ldx
        let mut ldx = HashMap::new();
        ldx.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xA2, affected_on_page: false });
        ldx.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xA6, affected_on_page: false });
        ldx.insert(AddressingMode::ZeropageIndexedY, ShortEntry{ cycles: 4, size: 2, opcode: 0xB6, affected_on_page: false });
        ldx.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xAE, affected_on_page: false });
        ldx.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0xBE, affected_on_page: true });
        instrs.insert(String::from("ldx"), ldx);

        // ldy
        let mut ldy = HashMap::new();
        ldy.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xA0, affected_on_page: false });
        ldy.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xA4, affected_on_page: false });
        ldy.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0xB4, affected_on_page: false });
        ldy.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xAC, affected_on_page: false });
        ldy.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0xBC, affected_on_page: true });
        instrs.insert(String::from("ldy"), ldy);

        // lsr
        let mut lsr = HashMap::new();
        lsr.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x4A, affected_on_page: false });
        lsr.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 5, size: 2, opcode: 0x46, affected_on_page: false });
        lsr.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 6, size: 2, opcode: 0x56, affected_on_page: false });
        lsr.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0x4E, affected_on_page: false });
        lsr.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 7, size: 3, opcode: 0x5E, affected_on_page: false });
        instrs.insert(String::from("lsr"), lsr);

        // nop
        let mut nop = HashMap::new();
        nop.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xEA, affected_on_page: false });
        instrs.insert(String::from("nop"), nop);

        // ora
        let mut ora = HashMap::new();
        ora.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0x09, affected_on_page: false });
        ora.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x05, affected_on_page: false });
        ora.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x15, affected_on_page: false });
        ora.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x0D, affected_on_page: false });
        ora.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0x1D, affected_on_page: true });
        ora.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0x19, affected_on_page: true });
        ora.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0x01, affected_on_page: false });
        ora.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0x11, affected_on_page: true });
        instrs.insert(String::from("ora"), ora);

        // pha
        let mut pha = HashMap::new();
        pha.insert(AddressingMode::Implied, ShortEntry{ cycles: 3, size: 1, opcode: 0x48, affected_on_page: false });
        instrs.insert(String::from("pha"), pha);

        // php
        let mut php = HashMap::new();
        php.insert(AddressingMode::Implied, ShortEntry{ cycles: 3, size: 1, opcode: 0x08, affected_on_page: false });
        instrs.insert(String::from("php"), php);

        // pla
        let mut pla = HashMap::new();
        pla.insert(AddressingMode::Implied, ShortEntry{ cycles: 4, size: 1, opcode: 0x68, affected_on_page: false });
        instrs.insert(String::from("pla"), pla);

        // plp
        let mut plp = HashMap::new();
        plp.insert(AddressingMode::Implied, ShortEntry{ cycles: 4, size: 1, opcode: 0x28, affected_on_page: false });
        instrs.insert(String::from("plp"), plp);

        // rol
        let mut rol = HashMap::new();
        rol.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x2A, affected_on_page: false });
        rol.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 5, size: 2, opcode: 0x26, affected_on_page: false });
        rol.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 6, size: 2, opcode: 0x36, affected_on_page: false });
        rol.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0x2E, affected_on_page: false });
        rol.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 7, size: 3, opcode: 0x3E, affected_on_page: false });
        instrs.insert(String::from("rol"), rol);

        // ror
        let mut ror = HashMap::new();
        ror.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x6A, affected_on_page: false });
        ror.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 5, size: 2, opcode: 0x66, affected_on_page: false });
        ror.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 6, size: 2, opcode: 0x76, affected_on_page: false });
        ror.insert(AddressingMode::Absolute, ShortEntry{ cycles: 6, size: 3, opcode: 0x6E, affected_on_page: false });
        ror.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 7, size: 3, opcode: 0x7E, affected_on_page: false });
        instrs.insert(String::from("ror"), ror);

        // rti
        let mut rti = HashMap::new();
        rti.insert(AddressingMode::Implied, ShortEntry{ cycles: 6, size: 1, opcode: 0x40, affected_on_page: false });
        instrs.insert(String::from("rti"), rti);

        // rts
        let mut rts = HashMap::new();
        rts.insert(AddressingMode::Implied, ShortEntry{ cycles: 6, size: 1, opcode: 0x60, affected_on_page: false });
        instrs.insert(String::from("rts"), rts);

        // sbc
        let mut sbc = HashMap::new();
        sbc.insert(AddressingMode::Immediate, ShortEntry{ cycles: 2, size: 2, opcode: 0xE9, affected_on_page: false });
        sbc.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0xE5, affected_on_page: false });
        sbc.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0xF5, affected_on_page: false });
        sbc.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0xED, affected_on_page: false });
        sbc.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 4, size: 3, opcode: 0xFD, affected_on_page: true });
        sbc.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 4, size: 3, opcode: 0xF9, affected_on_page: true });
        sbc.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0xE1, affected_on_page: false });
        sbc.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 5, size: 2, opcode: 0xF1, affected_on_page: true });
        instrs.insert(String::from("sbc"), sbc);

        // sec
        let mut sec = HashMap::new();
        sec.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x38, affected_on_page: false });
        instrs.insert(String::from("sec"), sec);

        // sed
        let mut sed = HashMap::new();
        sed.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xF8, affected_on_page: false });
        instrs.insert(String::from("sed"), sed);

        // sei
        let mut sei = HashMap::new();
        sei.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x78, affected_on_page: false });
        instrs.insert(String::from("sei"), sei);

        // sta
        let mut sta = HashMap::new();
        sta.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x85, affected_on_page: false });
        sta.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x95, affected_on_page: false });
        sta.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x8D, affected_on_page: false });
        sta.insert(AddressingMode::IndexedX, ShortEntry{ cycles: 5, size: 3, opcode: 0x9D, affected_on_page: false });
        sta.insert(AddressingMode::IndexedY, ShortEntry{ cycles: 5, size: 3, opcode: 0x99, affected_on_page: false });
        sta.insert(AddressingMode::IndirectX, ShortEntry{ cycles: 6, size: 2, opcode: 0x81, affected_on_page: false });
        sta.insert(AddressingMode::IndirectY, ShortEntry{ cycles: 6, size: 2, opcode: 0x91, affected_on_page: false });
        instrs.insert(String::from("sta"), sta);

        // stx
        let mut stx = HashMap::new();
        stx.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x86, affected_on_page: false });
        stx.insert(AddressingMode::ZeropageIndexedY, ShortEntry{ cycles: 4, size: 2, opcode: 0x96, affected_on_page: false });
        stx.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x8E, affected_on_page: false });
        instrs.insert(String::from("stx"), stx);

        // sty
        let mut sty = HashMap::new();
        sty.insert(AddressingMode::RelativeOrZeropage, ShortEntry{ cycles: 3, size: 2, opcode: 0x84, affected_on_page: false });
        sty.insert(AddressingMode::ZeropageIndexedX, ShortEntry{ cycles: 4, size: 2, opcode: 0x94, affected_on_page: false });
        sty.insert(AddressingMode::Absolute, ShortEntry{ cycles: 4, size: 3, opcode: 0x8C, affected_on_page: false });
        instrs.insert(String::from("sty"), sty);

        // tax
        let mut tax = HashMap::new();
        tax.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xAA, affected_on_page: false });
        instrs.insert(String::from("tax"), tax);

        // tay
        let mut tay = HashMap::new();
        tay.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xA8, affected_on_page: false });
        instrs.insert(String::from("tay"), tay);

        // tsx
        let mut tsx = HashMap::new();
        tsx.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0xBA, affected_on_page: false });
        instrs.insert(String::from("tsx"), tsx);

        // txa
        let mut txa = HashMap::new();
        txa.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x8A, affected_on_page: false });
        instrs.insert(String::from("txa"), txa);

        // txs
        let mut txs = HashMap::new();
        txs.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x9A, affected_on_page: false });
        instrs.insert(String::from("txs"), txs);

        // tya
        let mut tya = HashMap::new();
        tya.insert(AddressingMode::Implied, ShortEntry{ cycles: 2, size: 1, opcode: 0x98, affected_on_page: false });
        instrs.insert(String::from("tya"), tya);

        instrs
    };

    // pub static ref OPCODES: HashMap<u8, Entry> = {
    //     let mut opcodes = HashMap::new();

    //     // adc
    //     opcodes.insert(0x69, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("adc"), cycles: 2, size: 2, opcode: 0x69, affected_on_page: false });
    //     opcodes.insert(0x65, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("adc"), cycles: 3, size: 2, opcode: 0x65, affected_on_page: false });
    //     opcodes.insert(0x75, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("adc"), cycles: 4, size: 2, opcode: 0x75, affected_on_page: false });
    //     opcodes.insert(0x7D, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("adc"), cycles: 4, size: 3, opcode: 0x7D, affected_on_page: true });
    //     opcodes.insert(0x79, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("adc"), cycles: 4, size: 3, opcode: 0x79, affected_on_page: true });
    //     opcodes.insert(0x61, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("adc"), cycles: 6, size: 2, opcode: 0x61, affected_on_page: false });
    //     opcodes.insert(0x71, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("adc"), cycles: 5, size: 2, opcode: 0x71, affected_on_page: true });
    //     opcodes.insert(0x6D, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("adc"), cycles: 4, size: 3, opcode: 0x6D, affected_on_page: false });

    //     // and
    //     opcodes.insert(0x29, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("and"), cycles: 2, size: 2, opcode: 0x29, affected_on_page: false });
    //     opcodes.insert(0x25, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("and"), cycles: 3, size: 2, opcode: 0x25, affected_on_page: false });
    //     opcodes.insert(0x35, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("and"), cycles: 4, size: 2, opcode: 0x35, affected_on_page: false });
    //     opcodes.insert(0x2D, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("and"), cycles: 4, size: 3, opcode: 0x2D, affected_on_page: false });
    //     opcodes.insert(0x3D, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("and"), cycles: 4, size: 3, opcode: 0x3D, affected_on_page: true });
    //     opcodes.insert(0x39, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("and"), cycles: 4, size: 3, opcode: 0x39, affected_on_page: true });
    //     opcodes.insert(0x21, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("and"), cycles: 6, size: 2, opcode: 0x21, affected_on_page: false });
    //     opcodes.insert(0x31, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("and"), cycles: 5, size: 2, opcode: 0x31, affected_on_page: true });

    //     // asl
    //     opcodes.insert(0x0A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("asl"), cycles: 2, size: 1, opcode: 0x0A, affected_on_page: false });
    //     opcodes.insert(0x06, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("asl"), cycles: 5, size: 2, opcode: 0x06, affected_on_page: false });
    //     opcodes.insert(0x16, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("asl"), cycles: 6, size: 2, opcode: 0x16, affected_on_page: false });
    //     opcodes.insert(0x0E, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("asl"), cycles: 6, size: 3, opcode: 0x0E, affected_on_page: false });
    //     opcodes.insert(0x1E, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("asl"), cycles: 7, size: 3, opcode: 0x1E, affected_on_page: false });

    //     // bcc
    //     opcodes.insert(0x90, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bcc"), cycles: 2, size: 2, opcode: 0x90, affected_on_page: false });

    //     // bcs
    //     opcodes.insert(0xB0, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bcs"), cycles: 2, size: 2, opcode: 0xB0, affected_on_page: false });

    //     // beq
    //     opcodes.insert(0xF0, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("beq"), cycles: 2, size: 2, opcode: 0xF0, affected_on_page: true });

    //     // bit
    //     opcodes.insert(0x24, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bit"), cycles: 3, size: 2, opcode: 0x24, affected_on_page: false });
    //     opcodes.insert(0x2C, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("bit"), cycles: 4, size: 3, opcode: 0x2C, affected_on_page: false });

    //     // bmi
    //     opcodes.insert(0x30, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bmi"), cycles: 2, size: 2, opcode: 0x30, affected_on_page: false });

    //     // bne
    //     opcodes.insert(0xD0, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bne"), cycles: 2, size: 2, opcode: 0xD0, affected_on_page: false });

    //     // bpl
    //     opcodes.insert(0x10, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bpl"), cycles: 2, size: 2, opcode: 0x10, affected_on_page: false });

    //     // brk
    //     opcodes.insert(0x00, Entry { mode: AddressingMode::Implied, mnemonic: String::from("brk"), cycles: 7, size: 1, opcode: 0x00, affected_on_page: false });

    //     // bvc
    //     opcodes.insert(0x50, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bvc"), cycles: 2, size: 2, opcode: 0x50, affected_on_page: false });

    //     // bvs
    //     opcodes.insert(0x70, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("bvs"), cycles: 2, size: 2, opcode: 0x70, affected_on_page: false });

    //     // clc
    //     opcodes.insert(0x18, Entry { mode: AddressingMode::Implied, mnemonic: String::from("clc"), cycles: 2, size: 1, opcode: 0x18, affected_on_page: false });

    //     // cld
    //     opcodes.insert(0xD8, Entry { mode: AddressingMode::Implied, mnemonic: String::from("cld"), cycles: 2, size: 1, opcode: 0xD8, affected_on_page: false });

    //     // cli
    //     opcodes.insert(0x58, Entry { mode: AddressingMode::Implied, mnemonic: String::from("cli"), cycles: 2, size: 1, opcode: 0x58, affected_on_page: false });

    //     // clv
    //     opcodes.insert(0xB8, Entry { mode: AddressingMode::Implied, mnemonic: String::from("clv"), cycles: 2, size: 1, opcode: 0xB8, affected_on_page: false });

    //     // cmp
    //     opcodes.insert(0xC9, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("cmp"), cycles: 2, size: 2, opcode: 0xC9, affected_on_page: false });
    //     opcodes.insert(0xC5, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("cmp"), cycles: 3, size: 2, opcode: 0xC5, affected_on_page: false });
    //     opcodes.insert(0xD5, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("cmp"), cycles: 4, size: 2, opcode: 0xD5, affected_on_page: false });
    //     opcodes.insert(0xCD, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("cmp"), cycles: 4, size: 3, opcode: 0xCD, affected_on_page: false });
    //     opcodes.insert(0xDD, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("cmp"), cycles: 4, size: 3, opcode: 0xDD, affected_on_page: true });
    //     opcodes.insert(0xD9, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("cmp"), cycles: 4, size: 3, opcode: 0xD9, affected_on_page: true });
    //     opcodes.insert(0xC1, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("cmp"), cycles: 6, size: 2, opcode: 0xC1, affected_on_page: false });
    //     opcodes.insert(0xD1, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("cmp"), cycles: 5, size: 2, opcode: 0xD1, affected_on_page: true });

    //     // cpx
    //     opcodes.insert(0xE0, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("cpx"), cycles: 2, size: 2, opcode: 0xE0, affected_on_page: false });
    //     opcodes.insert(0xE4, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("cpx"), cycles: 3, size: 2, opcode: 0xE4, affected_on_page: false });
    //     opcodes.insert(0xEC, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("cpx"), cycles: 4, size: 3, opcode: 0xEC, affected_on_page: false });

    //     // cpy
    //     opcodes.insert(0xC0, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("cpy"), cycles: 2, size: 2, opcode: 0xC0, affected_on_page: false });
    //     opcodes.insert(0xC4, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("cpy"), cycles: 3, size: 2, opcode: 0xC4, affected_on_page: false });
    //     opcodes.insert(0xCC, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("cpy"), cycles: 4, size: 3, opcode: 0xCC, affected_on_page: false });

    //     // dec
    //     opcodes.insert(0xC6, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("dec"), cycles: 5, size: 2, opcode: 0xC6, affected_on_page: false });
    //     opcodes.insert(0xD6, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("dec"), cycles: 6, size: 2, opcode: 0xD6, affected_on_page: false });
    //     opcodes.insert(0xCE, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("dec"), cycles: 6, size: 3, opcode: 0xCE, affected_on_page: false });
    //     opcodes.insert(0xDE, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("dec"), cycles: 7, size: 3, opcode: 0xDE, affected_on_page: false });

    //     // dex
    //     opcodes.insert(0xCA, Entry { mode: AddressingMode::Implied, mnemonic: String::from("dex"), cycles: 2, size: 1, opcode: 0xCA, affected_on_page: false });

    //     // dey
    //     opcodes.insert(0x88, Entry { mode: AddressingMode::Implied, mnemonic: String::from("dey"), cycles: 2, size: 1, opcode: 0x88, affected_on_page: false });

    //     // eor
    //     opcodes.insert(0x49, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("eor"), cycles: 2, size: 2, opcode: 0x49, affected_on_page: false });
    //     opcodes.insert(0x45, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("eor"), cycles: 3, size: 2, opcode: 0x45, affected_on_page: false });
    //     opcodes.insert(0x55, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("eor"), cycles: 4, size: 2, opcode: 0x55, affected_on_page: false });
    //     opcodes.insert(0x4D, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("eor"), cycles: 4, size: 3, opcode: 0x4D, affected_on_page: false });
    //     opcodes.insert(0x5D, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("eor"), cycles: 4, size: 3, opcode: 0x5D, affected_on_page: true });
    //     opcodes.insert(0x59, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("eor"), cycles: 4, size: 3, opcode: 0x59, affected_on_page: true });
    //     opcodes.insert(0x41, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("eor"), cycles: 6, size: 2, opcode: 0x41, affected_on_page: false });
    //     opcodes.insert(0x51, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("eor"), cycles: 5, size: 2, opcode: 0x51, affected_on_page: true });

    //     // inc
    //     opcodes.insert(0xE6, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("inc"), cycles: 5, size: 2, opcode: 0xE6, affected_on_page: false });
    //     opcodes.insert(0xF6, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("inc"), cycles: 6, size: 2, opcode: 0xF6, affected_on_page: false });
    //     opcodes.insert(0xEE, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("inc"), cycles: 6, size: 3, opcode: 0xEE, affected_on_page: false });
    //     opcodes.insert(0xFE, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("inc"), cycles: 7, size: 3, opcode: 0xFE, affected_on_page: false });

    //     // inx
    //     opcodes.insert(0xE8, Entry { mode: AddressingMode::Implied, mnemonic: String::from("inx"), cycles: 2, size: 1, opcode: 0xE8, affected_on_page: false });

    //     // iny
    //     opcodes.insert(0xC8, Entry { mode: AddressingMode::Implied, mnemonic: String::from("iny"), cycles: 2, size: 1, opcode: 0xC8, affected_on_page: false });

    //     // jmp
    //     opcodes.insert(0x4C, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("jmp"), cycles: 3, size: 3, opcode: 0x4C, affected_on_page: false });
    //     opcodes.insert(0x6C, Entry { mode: AddressingMode::Indirect, mnemonic: String::from("jmp"), cycles: 5, size: 3, opcode: 0x6C, affected_on_page: false });

    //     // jsr
    //     opcodes.insert(0x20, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("jsr"), cycles: 6, size: 3, opcode: 0x20, affected_on_page: false });

    //     // lda
    //     opcodes.insert(0xA9, Entry{ mode: AddressingMode::Immediate, mnemonic: String::from("lda"), cycles: 2, size: 2, opcode: 0xA9, affected_on_page: false });
    //     opcodes.insert(0xA5, Entry{ mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("lda"), cycles: 3, size: 2, opcode: 0xA5, affected_on_page: false });
    //     opcodes.insert(0xB5, Entry{ mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("lda"), cycles: 4, size: 2, opcode: 0xB5, affected_on_page: false });
    //     opcodes.insert(0xAD, Entry{ mode: AddressingMode::Absolute, mnemonic: String::from("lda"), cycles: 4, size: 3, opcode: 0xAD, affected_on_page: false });
    //     opcodes.insert(0xBD, Entry{ mode: AddressingMode::IndexedX, mnemonic: String::from("lda"), cycles: 4, size: 3, opcode: 0xBD, affected_on_page: true });
    //     opcodes.insert(0xB9, Entry{ mode: AddressingMode::IndexedY, mnemonic: String::from("lda"), cycles: 4, size: 3, opcode: 0xB9, affected_on_page: true });
    //     opcodes.insert(0xA1, Entry{ mode: AddressingMode::IndirectX, mnemonic: String::from("lda"), cycles: 6, size: 2, opcode: 0xA1, affected_on_page: false });
    //     opcodes.insert(0xB1, Entry{ mode: AddressingMode::IndirectY, mnemonic: String::from("lda"), cycles: 5, size: 2, opcode: 0xB1, affected_on_page: true });

    //     // ldx
    //     opcodes.insert(0xA2, Entry{ mode: AddressingMode::Immediate, mnemonic: String::from("ldx"), cycles: 2, size: 2, opcode: 0xA2, affected_on_page: false });
    //     opcodes.insert(0xA6, Entry{ mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("ldx"), cycles: 3, size: 2, opcode: 0xA6, affected_on_page: false });
    //     opcodes.insert(0xB6, Entry{ mode: AddressingMode::ZeropageIndexedY, mnemonic: String::from("ldx"), cycles: 4, size: 2, opcode: 0xB6, affected_on_page: false });
    //     opcodes.insert(0xAE, Entry{ mode: AddressingMode::Absolute, mnemonic: String::from("ldx"), cycles: 4, size: 3, opcode: 0xAE, affected_on_page: false });
    //     opcodes.insert(0xBE, Entry{ mode: AddressingMode::IndexedY, mnemonic: String::from("ldx"), cycles: 4, size: 3, opcode: 0xBE, affected_on_page: true });

    //     // ldy
    //     opcodes.insert(0xA0, Entry{ mode: AddressingMode::Immediate, mnemonic: String::from("ldy"), cycles: 2, size: 2, opcode: 0xA0, affected_on_page: false });
    //     opcodes.insert(0xA4, Entry{ mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("ldy"), cycles: 3, size: 2, opcode: 0xA4, affected_on_page: false });
    //     opcodes.insert(0xB4, Entry{ mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("ldy"), cycles: 4, size: 2, opcode: 0xB4, affected_on_page: false });
    //     opcodes.insert(0xAC, Entry{ mode: AddressingMode::Absolute, mnemonic: String::from("ldy"), cycles: 4, size: 3, opcode: 0xAC, affected_on_page: false });
    //     opcodes.insert(0xBC, Entry{ mode: AddressingMode::IndexedX, mnemonic: String::from("ldy"), cycles: 4, size: 3, opcode: 0xBC, affected_on_page: true });

    //     // lsr
    //     opcodes.insert(0x4A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("lsr"), cycles: 2, size: 1, opcode: 0x4A, affected_on_page: false });
    //     opcodes.insert(0x46, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("lsr"), cycles: 5, size: 2, opcode: 0x46, affected_on_page: false });
    //     opcodes.insert(0x56, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("lsr"), cycles: 6, size: 2, opcode: 0x56, affected_on_page: false });
    //     opcodes.insert(0x4E, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("lsr"), cycles: 6, size: 3, opcode: 0x4E, affected_on_page: false });
    //     opcodes.insert(0x5E, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("lsr"), cycles: 7, size: 3, opcode: 0x5E, affected_on_page: false });

    //     // nop
    //     opcodes.insert(0xEA, Entry { mode: AddressingMode::Implied, mnemonic: String::from("nop"), cycles: 2, size: 1, opcode: 0xEA, affected_on_page: false });

    //     // ora
    //     opcodes.insert(0x09, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("ora"), cycles: 2, size: 2, opcode: 0x09, affected_on_page: false });
    //     opcodes.insert(0x05, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("ora"), cycles: 3, size: 2, opcode: 0x05, affected_on_page: false });
    //     opcodes.insert(0x15, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("ora"), cycles: 4, size: 2, opcode: 0x15, affected_on_page: false });
    //     opcodes.insert(0x0D, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("ora"), cycles: 4, size: 3, opcode: 0x0D, affected_on_page: false });
    //     opcodes.insert(0x1D, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("ora"), cycles: 4, size: 3, opcode: 0x1D, affected_on_page: true });
    //     opcodes.insert(0x19, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("ora"), cycles: 4, size: 3, opcode: 0x19, affected_on_page: true });
    //     opcodes.insert(0x01, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("ora"), cycles: 6, size: 2, opcode: 0x01, affected_on_page: false });
    //     opcodes.insert(0x11, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("ora"), cycles: 5, size: 2, opcode: 0x11, affected_on_page: true });

    //     // pha
    //     opcodes.insert(0x48, Entry { mode: AddressingMode::Implied, mnemonic: String::from("pha"), cycles: 3, size: 1, opcode: 0x48, affected_on_page: false });

    //     // php
    //     opcodes.insert(0x08, Entry { mode: AddressingMode::Implied, mnemonic: String::from("php"), cycles: 3, size: 1, opcode: 0x08, affected_on_page: false });

    //     // pla
    //     opcodes.insert(0x68, Entry { mode: AddressingMode::Implied, mnemonic: String::from("pla"), cycles: 4, size: 1, opcode: 0x68, affected_on_page: false });

    //     // plp
    //     opcodes.insert(0x28, Entry { mode: AddressingMode::Implied, mnemonic: String::from("plp"), cycles: 4, size: 1, opcode: 0x28, affected_on_page: false });

    //     // rol
    //     opcodes.insert(0x2A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("rol"), cycles: 2, size: 1, opcode: 0x2A, affected_on_page: false });
    //     opcodes.insert(0x26, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("rol"), cycles: 5, size: 2, opcode: 0x26, affected_on_page: false });
    //     opcodes.insert(0x36, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("rol"), cycles: 6, size: 2, opcode: 0x36, affected_on_page: false });
    //     opcodes.insert(0x2E, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("rol"), cycles: 6, size: 3, opcode: 0x2E, affected_on_page: false });
    //     opcodes.insert(0x3E, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("rol"), cycles: 7, size: 3, opcode: 0x3E, affected_on_page: false });

    //     // ror
    //     opcodes.insert(0x6A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("ror"), cycles: 2, size: 1, opcode: 0x6A, affected_on_page: false });
    //     opcodes.insert(0x66, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("ror"), cycles: 5, size: 2, opcode: 0x66, affected_on_page: false });
    //     opcodes.insert(0x76, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("ror"), cycles: 6, size: 2, opcode: 0x76, affected_on_page: false });
    //     opcodes.insert(0x6E, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("ror"), cycles: 6, size: 3, opcode: 0x6E, affected_on_page: false });
    //     opcodes.insert(0x7E, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("ror"), cycles: 7, size: 3, opcode: 0x7E, affected_on_page: false });

    //     // rti
    //     opcodes.insert(0x40, Entry { mode: AddressingMode::Implied, mnemonic: String::from("rti"), cycles: 6, size: 1, opcode: 0x40, affected_on_page: false });

    //     // rts
    //     opcodes.insert(0x60, Entry { mode: AddressingMode::Implied, mnemonic: String::from("rts"), cycles: 6, size: 1, opcode: 0x60, affected_on_page: false });

    //     // sbc
    //     opcodes.insert(0xE9, Entry { mode: AddressingMode::Immediate, mnemonic: String::from("sbc"), cycles: 2, size: 2, opcode: 0xE9, affected_on_page: false });
    //     opcodes.insert(0xE5, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("sbc"), cycles: 3, size: 2, opcode: 0xE5, affected_on_page: false });
    //     opcodes.insert(0xF5, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("sbc"), cycles: 4, size: 2, opcode: 0xF5, affected_on_page: false });
    //     opcodes.insert(0xED, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("sbc"), cycles: 4, size: 3, opcode: 0xED, affected_on_page: false });
    //     opcodes.insert(0xFD, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("sbc"), cycles: 4, size: 3, opcode: 0xFD, affected_on_page: true });
    //     opcodes.insert(0xF9, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("sbc"), cycles: 4, size: 3, opcode: 0xF9, affected_on_page: true });
    //     opcodes.insert(0xE1, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("sbc"), cycles: 6, size: 2, opcode: 0xE1, affected_on_page: false });
    //     opcodes.insert(0xF1, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("sbc"), cycles: 5, size: 2, opcode: 0xF1, affected_on_page: true });

    //     // sec
    //     opcodes.insert(0x38, Entry{ mode: AddressingMode::Implied, mnemonic: String::from("sec"), cycles: 2, size: 1, opcode: 0x38, affected_on_page: false });

    //     // sed
    //     opcodes.insert(0xF8, Entry{ mode: AddressingMode::Implied, mnemonic: String::from("sed"), cycles: 2, size: 1, opcode: 0xF8, affected_on_page: false });

    //     // sei
    //     opcodes.insert(0x78, Entry{ mode: AddressingMode::Implied, mnemonic: String::from("sei"), cycles: 2, size: 1, opcode: 0x78, affected_on_page: false });

    //     // sta
    //     opcodes.insert(0x85, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("sta"), cycles: 3, size: 2, opcode: 0x85, affected_on_page: false });
    //     opcodes.insert(0x95, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("sta"), cycles: 4, size: 2, opcode: 0x95, affected_on_page: false });
    //     opcodes.insert(0x8D, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("sta"), cycles: 4, size: 3, opcode: 0x8D, affected_on_page: false });
    //     opcodes.insert(0x9D, Entry { mode: AddressingMode::IndexedX, mnemonic: String::from("sta"), cycles: 5, size: 3, opcode: 0x9D, affected_on_page: false });
    //     opcodes.insert(0x99, Entry { mode: AddressingMode::IndexedY, mnemonic: String::from("sta"), cycles: 5, size: 3, opcode: 0x99, affected_on_page: false });
    //     opcodes.insert(0x81, Entry { mode: AddressingMode::IndirectX, mnemonic: String::from("sta"), cycles: 6, size: 2, opcode: 0x81, affected_on_page: false });
    //     opcodes.insert(0x91, Entry { mode: AddressingMode::IndirectY, mnemonic: String::from("sta"), cycles: 6, size: 2, opcode: 0x91, affected_on_page: false });

    //     // stx
    //     opcodes.insert(0x86, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("stx"), cycles: 3, size: 2, opcode: 0x86, affected_on_page: false });
    //     opcodes.insert(0x96, Entry { mode: AddressingMode::ZeropageIndexedY, mnemonic: String::from("stx"), cycles: 4, size: 2, opcode: 0x96, affected_on_page: false });
    //     opcodes.insert(0x8E, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("stx"), cycles: 4, size: 3, opcode: 0x8E, affected_on_page: false });

    //     // sty
    //     opcodes.insert(0x84, Entry { mode: AddressingMode::RelativeOrZeropage, mnemonic: String::from("sty"), cycles: 3, size: 2, opcode: 0x84, affected_on_page: false });
    //     opcodes.insert(0x94, Entry { mode: AddressingMode::ZeropageIndexedX, mnemonic: String::from("sty"), cycles: 4, size: 2, opcode: 0x94, affected_on_page: false });
    //     opcodes.insert(0x8C, Entry { mode: AddressingMode::Absolute, mnemonic: String::from("sty"), cycles: 4, size: 3, opcode: 0x8C, affected_on_page: false });

    //     // tax
    //     opcodes.insert(0xAA, Entry{ mode: AddressingMode::Implied, mnemonic: String::from("tax"), cycles: 2, size: 1, opcode: 0xAA, affected_on_page: false });

    //     // tay
    //     opcodes.insert(0xA8, Entry { mode: AddressingMode::Implied, mnemonic: String::from("tay"), cycles: 2, size: 1, opcode: 0xA8, affected_on_page: false });

    //     // tsx
    //     opcodes.insert(0xBA, Entry { mode: AddressingMode::Implied, mnemonic: String::from("tsx"), cycles: 2, size: 1, opcode: 0xBA, affected_on_page: false });

    //     // txa
    //     opcodes.insert(0x8A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("txa"), cycles: 2, size: 1, opcode: 0x8A, affected_on_page: false });

    //     // txs
    //     opcodes.insert(0x9A, Entry { mode: AddressingMode::Implied, mnemonic: String::from("txs"), cycles: 2, size: 1, opcode: 0x9A, affected_on_page: false });

    //     // tya
    //     opcodes.insert(0x98, Entry { mode: AddressingMode::Implied, mnemonic: String::from("tya"), cycles: 2, size: 1, opcode: 0x98, affected_on_page: false });

    //     opcodes
    // };

    pub static ref CONTROL_FUNCTIONS: HashMap<String, Control> = {
        let mut functions = HashMap::new();

        functions.insert(String::from(".hibyte"), Control { control_type: ControlType::Hibyte, has_identifier: None, required_args: Some((1, 1)), touches_context: false });
        functions.insert(String::from(".lobyte"), Control { control_type: ControlType::Lobyte, has_identifier: None, required_args: Some((1, 1)), touches_context: false });
        functions.insert(String::from(".macro"), Control { control_type: ControlType::StartMacro, has_identifier: Some(false), required_args: None, touches_context: true });
        functions.insert(String::from(".proc"), Control { control_type: ControlType::StartProc, has_identifier: Some(false), required_args: Some((0, 0)), touches_context: true });
        functions.insert(String::from(".scope"), Control { control_type: ControlType::StartScope, has_identifier: Some(false), required_args: Some((0, 0)), touches_context: true });
        functions.insert(String::from(".endscope"), Control { control_type: ControlType::EndScope, has_identifier: None, required_args: Some((0, 0)), touches_context: true });
        functions.insert(String::from(".endproc"), Control { control_type: ControlType::EndProc, has_identifier: None, required_args: Some((0, 0)), touches_context: true });
        functions.insert(String::from(".endmacro"), Control { control_type: ControlType::EndMacro, has_identifier: None, required_args: Some((0, 0)), touches_context: true });
        functions.insert(String::from(".segment"), Control { control_type: ControlType::Segment, has_identifier: None, required_args: Some((1, 1)), touches_context: false });
        functions.insert(String::from(".byte"), Control { control_type: ControlType::Byte, has_identifier: None, required_args: None, touches_context: false });
        functions.insert(String::from(".db"), Control { control_type: ControlType::Byte, has_identifier: None, required_args: None, touches_context: false });
        functions.insert(String::from(".word"), Control { control_type: ControlType::Word, has_identifier: None, required_args: None, touches_context: false });
        functions.insert(String::from(".dw"), Control { control_type: ControlType::Word, has_identifier: None, required_args: None, touches_context: false });
        functions.insert(String::from(".addr"), Control { control_type: ControlType::Addr, has_identifier: None, required_args: None, touches_context: false });
        functions.insert(String::from(".incbin"), Control { control_type: ControlType::IncBin, has_identifier: None, required_args: Some((1, 1)), touches_context: false });
        functions.insert(String::from(".repeat"), Control { control_type: ControlType::StartRepeat, has_identifier: Some(true), required_args: Some((1, 2)), touches_context: true });
        functions.insert(String::from(".endrepeat"), Control { control_type: ControlType::EndRepeat, has_identifier: None, required_args: None, touches_context: true });
        functions.insert(String::from(".include"), Control { control_type: ControlType::IncludeSource, has_identifier: None, required_args: Some((1, 1)), touches_context: false });
        functions.insert(String::from(".res"), Control { control_type: ControlType::ReserveMemory, has_identifier: None, required_args: Some((1, 2)), touches_context: false });

        functions
    };
}
