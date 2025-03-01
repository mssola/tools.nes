/// Information for a file that has been parsed/assembled.
#[derive(Clone, Debug, PartialEq)]
pub struct SourceInfo {
    /// Directory where the file is located.
    pub directory: std::path::PathBuf,

    /// Friendly name for the given file.
    pub name: String,
}

impl Default for SourceInfo {
    fn default() -> Self {
        SourceInfo {
            directory: std::env::current_dir().unwrap().to_path_buf(),
            name: "".to_string(),
        }
    }
}

/// Error provides an interface used throughout this crate in which an error
/// message of type String is located through a line and an associated
/// SourceInfo. If global is set to true, then `line` does not matter.
#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    /// Line number where the error was found relative to the `source`. You can
    /// ignore this field if `global` is true.
    pub line: usize,

    /// Whether the error affects the whole file or not.
    pub global: bool,

    /// Information on the file where the error was found.
    pub source: SourceInfo,

    /// Human-readable representation of the error. Don't use this field
    /// directly, prefer its `std::fmt::Display` implementation. Hence, calling
    /// `.to_string()` on it is most probably the way to go.
    pub message: String,
}

impl From<Error> for Vec<Error> {
    fn from(err: Error) -> Self {
        vec![err]
    }
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.global {
            if self.source.name.is_empty() {
                write!(f, "{}", self.message)
            } else {
                write!(f, "{} ({})", self.message, self.source.name)
            }
        } else if self.source.name.is_empty() {
            write!(f, "{} (line {})", self.message, self.line + 1)
        } else {
            write!(
                f,
                "{} ({}: line {})",
                self.message,
                self.source.name,
                self.line + 1
            )
        }
    }
}

pub mod assembler;
pub mod node;
pub mod object;
pub mod opcodes;
pub mod parser;

mod cfg;
/// Mapping defines structures for laying out how the code will be assembled
/// both on memory and on the ROM file itself. Notice that we take a different
/// approach than 'cc65' because that compiler has to take into account a
/// myriad of machines that had the MOS 6502 processor. Here we have a clear
/// target and we can be more specific. That is, the community has settled on a
/// very specific ROM file format, and hence the "linker configuration" turns
/// out to be simpler. The code here takes it into consideration by defining
/// three regions on the ROM file:
///
///   1. The header (i.e. `SectionType::Header`) will be allocated exactly on
///      the first 16 bytes of the file. Moreover, the first 6 bytes are
///      absolutely mandatory to be filled by the programmer.
///   2. The PRG ROM (i.e. `SectionType::PrgRom`) will be allocated next to the
///      header (i.e. trainer support is not available on this assembler). This
///      section is laid out in blocks of 8KB and contains the program.
///   3. The CHR ROM (i.e. `SectionType::ChrRom`) will be allocated just after
///      the PRG ROM and is laid out in blocks of 4KB, containing the ROM data (if
///      available).
///
/// This is the layout for the ROM file itself, but it can itself be subdivided
/// into "mappings" (i.e. `Mapping`). A Mapping is a configuration inside of
/// one of these three sections. For a simple section like the header having
/// only one mapping will be enough, but for PRG ROM we might define a mapping
/// for each bank, for example. And even in simple scenarios, it's quite usual
/// to split this section at least with "CODE" (which contains the actual
/// program), and "VECTORS" (6 bytes containing the addresses for the vectors
/// for a 6502 processor). A Mapping contains quite a lot of info, but you can
/// think of it as a way to subdivide a section, defining stuff like where it
/// starts, its size, how to fill it if the programmer did not occupy the
/// region fully, etc.
///
/// Inside of a mapping there might be multiple segments. This is in turn a way
/// to further subdivide the memory region, and it defines contiguous space
/// inside of a mapping. This way, regardless of where you define a segment,
/// there are some guarantees on the order.
pub mod mapping;
