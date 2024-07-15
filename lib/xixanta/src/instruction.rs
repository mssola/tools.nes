use crate::errors::ParseError;
use std::fmt;
use std::ops::Range;

/// PString is a String with position information.
#[derive(Debug, Clone, PartialEq)]
pub struct PString {
    pub value: String,
    pub line: usize,
    pub range: Range<usize>,
}

impl PString {
    pub fn new() -> Self {
        PString {
            value: String::from(""),
            line: 0,
            range: Range { start: 0, end: 0 },
        }
    }

    pub fn from(value: &str) -> Self {
        PString {
            value: String::from(value),
            line: 0,
            range: Range { start: 0, end: 0 },
        }
    }

    pub fn parser_error(&self, message: &str) -> ParseError {
        // TODO: we can go further :)
        ParseError {
            line: self.line,
            message: String::from(message),
        }
    }

    pub fn is_reserved(&self) -> bool {
        matches!(self.value.to_lowercase().as_str(), "x" | "y" | "a")
    }
}

#[derive(Eq, Hash, PartialEq, Debug, Clone)]
pub enum AddressingMode {
    Unknown,
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
            _ => write!(f, "unknown"),
        }
    }
}

/// Encodable is a trait to be implemented by those structs that might need to
/// be encoded into the outside world. That is, structures that make sense to
/// output into files or other output streams.
pub trait Encodable {
    /// Returns a fixed array of bytes which belong to an encodable object. Note
    /// that the capacity is fixed, but the actual size must be checked with the
    /// `size` trait function, otherwise elements beyond that size might contain
    /// junk.
    fn to_bytes(&self) -> [u8; 3];

    /// Returns the actual size of the data returned by `to_bytes`.
    fn size(&self) -> u8;

    /// Returns a vector which contains the exact byte data for the given
    /// object. In contrast with `to_bytes`, the caller does not need to check
    /// for `size`: the returned vector is tailored to the exact amount of
    /// bytes for the object.
    fn to_hex(&self) -> Vec<String>;

    /// Returns a string representation which makes sense to a human (e.g.
    /// instead of providing the byte encoded opcode for an instruction, show
    /// the mnemonic).
    fn to_human(&self) -> String;

    /// Returns a string representation with higher verbosity than `to_human`.
    fn to_verbose(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub mnemonic: PString,
    pub opcode: u8,
    pub bytes: [u8; 2],
    pub size: u8,
    pub left: Option<PString>,
    pub right: Option<PString>,
    pub mode: AddressingMode,
    pub cycles: u8,
    pub affected_on_page: bool,
}

impl Instruction {
    pub fn unknown() -> Instruction {
        Instruction {
            mnemonic: PString::new(),
            opcode: 0,
            bytes: [0, 0],
            size: 0,
            left: None,
            right: None,
            mode: AddressingMode::Unknown,
            cycles: 0,
            affected_on_page: false,
        }
    }

    pub fn from(mnemonic: &str) -> Instruction {
        Instruction {
            mnemonic: PString::from(mnemonic),
            opcode: 0,
            bytes: [0, 0],
            size: 0,
            left: None,
            right: None,
            mode: AddressingMode::Unknown,
            cycles: 0,
            affected_on_page: false,
        }
    }
}

impl Encodable for Instruction {
    fn size(&self) -> u8 {
        self.size
    }

    fn to_hex(&self) -> Vec<String> {
        let mut ret = vec![];

        ret.push(format!("{:02X}", self.opcode));
        if self.size > 1 {
            ret.push(format!("{:02X}", self.bytes[0]));
        }
        if self.size == 3 {
            ret.push(format!("{:02X}", self.bytes[1]));
        }

        ret
    }

    fn to_bytes(&self) -> [u8; 3] {
        [self.opcode.to_le_bytes()[0], self.bytes[0], self.bytes[1]]
    }

    fn to_human(&self) -> String {
        match self.mode {
            AddressingMode::Implied => self.mnemonic.value.clone(),
            AddressingMode::Immediate => format!("{} #${:02X}", self.mnemonic.value, self.bytes[0]),
            AddressingMode::Absolute => format!(
                "{} ${:02X}{:02X}",
                self.mnemonic.value, self.bytes[1], self.bytes[0]
            ),
            AddressingMode::RelativeOrZeropage => {
                format!("{} ${:02X}", self.mnemonic.value, self.bytes[0])
            }
            AddressingMode::IndexedX => format!(
                "{} ${:02X}{:02X}, x",
                self.mnemonic.value, self.bytes[1], self.bytes[0]
            ),
            AddressingMode::IndexedY => format!(
                "{} ${:02X}{:02X}, y",
                self.mnemonic.value, self.bytes[1], self.bytes[0]
            ),
            AddressingMode::ZeropageIndexedX => {
                format!("{} ${:02X}, x", self.mnemonic.value, self.bytes[0])
            }
            AddressingMode::ZeropageIndexedY => {
                format!("{} ${:02X}, y", self.mnemonic.value, self.bytes[0])
            }
            AddressingMode::Indirect => format!(
                "{} (${:02X}{:02X})",
                self.mnemonic.value, self.bytes[1], self.bytes[0]
            ),
            AddressingMode::IndirectX => {
                format!("{} (${:02X}, x)", self.mnemonic.value, self.bytes[0])
            }
            AddressingMode::IndirectY => {
                format!("{} (${:02X}), y", self.mnemonic.value, self.bytes[0])
            }
            AddressingMode::Unknown => String::from("unknown instruction"),
        }
    }

    fn to_verbose(&self) -> String {
        format!("{:#?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generic {
    pub identifier: PString,
    pub left: Option<PString>,
    pub right: Option<PString>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scoped {
    pub identifier: PString,
    pub start: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub identifier: PString,
    pub bytes: [u8; 2],
    pub size: u8,
}

impl Encodable for Literal {
    fn size(&self) -> u8 {
        self.size
    }

    fn to_hex(&self) -> Vec<String> {
        let mut ret = vec![];

        ret.push(format!("{:02X}", self.bytes[0]));
        if self.size == 2 {
            ret.push(format!("{:02X}", self.bytes[1]));
        } else if self.size != 1 {
            panic!("size for literal should be either 1 or 2");
        }

        ret
    }

    fn to_bytes(&self) -> [u8; 3] {
        [self.bytes[0], self.bytes[1], 0]
    }

    fn to_human(&self) -> String {
        match self.size {
            1 => format!(".byte ${:02X}", self.bytes[0]),
            2 => format!(".byte ${:02X}{:02X}", self.bytes[1], self.bytes[0]),
            _ => String::from("unknown literal"),
        }
    }

    fn to_verbose(&self) -> String {
        format!("{:#?}", self)
    }
}

#[derive(Debug, Clone, Eq, Ord, PartialEq, PartialOrd)]
pub struct Fill {
    pub value: u8,
}

impl Encodable for Fill {
    fn size(&self) -> u8 {
        1
    }

    fn to_hex(&self) -> Vec<String> {
        let mut ret = vec![];
        ret.push(format!("{:02X}", self.value));

        ret
    }

    fn to_bytes(&self) -> [u8; 3] {
        [self.value, 0, 0]
    }

    fn to_human(&self) -> String {
        format!("${:02X}", self.value)
    }

    fn to_verbose(&self) -> String {
        format!("{:#?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Generic(Generic),
    Instruction(Instruction),
    Scoped(Scoped),
    Literal(Literal),
    Fill(Fill),
}

impl Node {
    pub fn is_encodeable(&self) -> bool {
        matches!(
            self,
            &Node::Instruction(_) | &Node::Literal(_) | &Node::Fill(_)
        )
    }
}
