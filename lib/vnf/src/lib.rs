use header::Header;
use std::assert_matches;
use std::collections::HashMap;
use std::fs::File;
use std::io::{ErrorKind, Read};
use std::ops::Range;
use std::path::Path;
use xixanta::opcodes::AddressingMode;
use xixanta::opcodes::{Instruction, InstructionIdentifier, OPCODES};

/// Values on the 'status' register converted to bools for easier use.
#[derive(Debug)]
pub struct StatusRegister {
    pub negative: bool,
    pub overflow: bool,
    pub brk: bool,
    pub decimal: bool,
    pub interrupt: bool,
    pub zero: bool,
    pub carry: bool,

    /// The byte after a 'brk' instruction.
    pub break_mark: u8,
}

impl Default for StatusRegister {
    fn default() -> Self {
        Self {
            negative: false,
            overflow: false,
            brk: false,
            decimal: false,
            interrupt: true,
            zero: false,
            carry: false,
            break_mark: 0,
        }
    }
}

impl StatusRegister {
    // Bitmap constants for how the different bools are laid out on the Status
    // Register. These are defined like so: (from most to least significant):
    //   N V - B D I Z C
    pub const CARRY: u8 = 1 << 0;
    pub const ZERO: u8 = 1 << 1;
    pub const INTERRUPT: u8 = 1 << 2;
    pub const DECIMAL: u8 = 1 << 3;
    pub const BRK: u8 = 1 << 4;
    // NOTE: bit 5 unused.
    pub const OVERFLOW: u8 = 1 << 6;
    pub const NEGATIVE: u8 = 1 << 7;

    /// Returns a string with the initial letter for each status bit that is
    /// set. Otherwise, for unset bits, a '-' is given.
    fn humanize(&self) -> String {
        let mut res = String::from("");

        if self.negative {
            res.push('N');
        } else {
            res.push('-');
        }
        if self.overflow {
            res.push('V');
        } else {
            res.push('-');
        }
        if self.brk {
            res.push('B');
        } else {
            res.push('-');
        }
        if self.decimal {
            res.push('D');
        } else {
            res.push('-');
        }
        if self.interrupt {
            res.push('I');
        } else {
            res.push('-');
        }
        if self.zero {
            res.push('Z');
        } else {
            res.push('-');
        }
        if self.carry {
            res.push('C');
        } else {
            res.push('-');
        }

        res.to_string()
    }
}

/// Registers from the APU chip.
#[derive(Debug, Default)]
pub struct APU {
    pub dmc: u8,
    pub frame_counter: u8,
}

/// Registers from the PPU chip.
#[derive(Debug, Default)]
pub struct PPU {
    pub control: u8,
    pub mask: u8,
    pub status: u8,
    pub scroll: u8,
    pub address: u8,
    pub data: u8,
    pub oam_address: u8,
    pub oam_dma: u8,
}

/// A byte from the memory, which other than the actual value, also contains
/// different stats for it.
#[derive(Clone, Copy, Debug, Default)]
pub struct MemoryCell {
    /// The current value.
    pub value: u8,

    /// Are writes allowed?
    pub write_allowed: bool,

    /// Are reads allowed?
    pub read_allowed: bool,

    /// How many writes have happened?
    pub writes: usize,

    /// How many reads have happened?
    pub reads: usize,
}

/// The initial value to be set for memory cells on a given execution.
#[derive(Debug)]
pub enum MemoryInitialValue {
    Fixed(u8),
    Random,
}

/// Allows users to define a policy for how the memory should be initialized for
/// the given Machine.
#[derive(Debug)]
pub struct MemoryPolicy {
    /// The initial value to be given for each cell.
    pub initial_value: MemoryInitialValue,

    /// The ranges for which reads are allowed to happen.
    pub allowed_reads: Vec<Range<usize>>,

    /// The ranges for which writes are allowed to happen.
    pub allowed_writes: Vec<Range<usize>>,

    /// How low can the stack go. Use this to control stack underflows.
    pub minimum_stack_value: u8,
}

impl Default for MemoryPolicy {
    fn default() -> Self {
        #[allow(clippy::single_range_in_vec_init)]
        Self {
            initial_value: MemoryInitialValue::Fixed(0),
            allowed_reads: vec![(0..0x800)],
            allowed_writes: vec![(0..0x800)],
            minimum_stack_value: 0,
        }
    }
}

/// The state of the Joypad handshake process.
#[derive(Debug)]
pub enum JoypadState {
    /// The Joypad is waiting for the read handshake to happen.
    Waiting,

    /// The joypad received the first byte on the read handshake. Now it's
    /// waiting for the other byte to be written to switch to the 'Sending'
    /// state.
    Received,

    /// The joypad can be read and has data available to it.
    Sending,
}

/// The state of a Joypad.
///
/// NOTE: for now only the standard joypad is supported.
#[derive(Debug)]
pub struct Joypad {
    /// The current state of the joypad.
    pub state: JoypadState,

    /// Queue of pending input values.
    pub values: Vec<u8>,

    /// The shift register. Whenever the joypad goes from the 'Received' state
    /// to 'Sending', the first input in 'values' is removed and set to this
    /// register. Then all reads will directly happen from this register.
    pub shift: u8,

    /// How many times the shift register has been read. When reaching its
    /// limit, any subsequent reads will fail.
    pub reads: u8,
}

impl Default for Joypad {
    fn default() -> Self {
        Self {
            state: JoypadState::Waiting,
            values: vec![],
            shift: 0,
            reads: 0,
        }
    }
}

impl Joypad {
    /// Use these constants to prepare the inputs for functions such as
    /// push_inputs().
    ///
    /// NOTE: the specific values here are sorted in reverse order. That is
    /// because the consumer expects to get each bit from the shift chip in
    /// reverse order: from least to most significant bits.
    pub const BUTTON_RIGHT: u8 = 1 << 7;
    pub const BUTTON_LEFT: u8 = 1 << 6;
    pub const BUTTON_DOWN: u8 = 1 << 5;
    pub const BUTTON_UP: u8 = 1 << 4;
    pub const BUTTON_START: u8 = 1 << 3;
    pub const BUTTON_SELECT: u8 = 1 << 2;
    pub const BUTTON_B: u8 = 1 << 1;
    pub const BUTTON_A: u8 = 1 << 0;

    /// Initialize the Joypad so it's ready to accept reads. Call this function
    /// whenever the read handshake has been completed.
    pub fn prepare_for_reads(&mut self) -> Result<(), String> {
        if self.values.is_empty() {
            return Err("no more input on the queue".to_string());
        }
        self.reads = 0;

        // NOTE: flip the bits as the consumer actually expects a 1 for
        // unpressed and a 0 for pressed. This is not done directly in the
        // 'BUTTON_*' constants out of convenience from the API point of view.
        self.shift = !self.values.remove(0);

        Ok(())
    }

    /// Push the given 'inputs' to the queue.
    pub fn push_inputs(&mut self, inputs: &[u8]) {
        self.values.extend_from_slice(inputs);
    }

    /// Remove all pending inputs from the queue.
    pub fn clear(&mut self) {
        self.values.clear();
    }
}

/// The core structure for the virtual machine. Use this structure to read and
/// run a ROM file. At each step it will hold the current status of the machine
/// so it can be inspected programatically in full detail.
#[derive(Debug)]
pub struct Machine {
    /// Is the machine active at all?
    active: bool,

    /// Whether the machine is supposed to be running just a function (while
    /// also going into inner calls). Hence, it will stop whenever an 'rts' or
    /// 'rti' instruction is found at the top level.
    pub run_function_mode: bool,

    /// The PRG ROM pool of bytes.
    pub prg_rom: Vec<u8>,

    /// The advertised size of PRG ROM. That is, regardless of the length the
    /// ->prg_rom, what's the supposed size of PRG ROM.
    pub prg_rom_size: usize,

    /// What is the instruction that is to be run.
    pub current_instruction: Instruction,

    /// The number of cycles that the CPU has consumed.
    pub cycles: usize,

    /// The extra cycles to be added to the ones inherent of the current
    /// instruction.
    extra_cycles: usize,

    /// The amount of cycles wasted on page penalties.
    pub page_penalty: usize,

    /// Number of instructions that have been run so far.
    pub instructions: usize,

    /// The program counter register.
    pub pc: usize,

    /// Whether the program counter should be left untouched as the current
    /// instruction already messed with it.
    skip_pc: bool,

    /// The status register.
    pub status_register: StatusRegister,

    /// The RAM for this machine. Each memory cell contains a value, but also
    /// read/write policies and statistics.
    pub ram: Vec<MemoryCell>,

    /// The 'a' register.
    pub a: u8,

    /// The 'x' register.
    pub x: u8,

    /// The 'y' register.
    pub y: u8,

    /// The 's' register.
    pub s: u8,

    /// Status from the APU chip.
    pub apu: APU,

    /// Status from the PPU chip.
    pub ppu: PPU,

    /// Whether the run should be verbose, sending to standard output statistics
    /// for each instruction being run, the stack, etc.
    pub verbose: bool,

    /// When 'verbose' is true, whether the APU should also be included into the
    /// output. This is enabled/disabled automatically whenever the machine
    /// detects a change on the APU.
    should_report_apu: bool,

    /// When 'verbose' is true, whether the PPU should also be included into the
    /// output. This is enabled/disabled automatically whenever the machine
    /// detects a change on the PPU.
    should_report_ppu: bool,

    /// The initial value for the stack register. Used to detect stack
    /// under/over flows.
    initial_stack_value: u8,

    /// The memory policy for this machine. That is, the value to be used as the
    /// default for each cell, which regions are allowed for read/writes, etc.
    policy: MemoryPolicy,

    /// The status of both Joypads.
    joypads: [Joypad; 2],
}

// Returns a vector of MemoryCell representing the RAM for a Machine, which
// follows the memory policy as defined in 'policy'.
fn init_memory(policy: &MemoryPolicy) -> Vec<MemoryCell> {
    let mut vec = Vec::with_capacity(0x800);

    for i in 0..0x800 {
        let read_allowed = policy.allowed_reads.iter().any(|range| range.contains(&i));
        let write_allowed = policy.allowed_writes.iter().any(|range| range.contains(&i));

        vec.push(MemoryCell {
            value: match policy.initial_value {
                MemoryInitialValue::Fixed(n) => n,
                MemoryInitialValue::Random => rand::random(),
            },
            write_allowed,
            read_allowed,
            reads: 0,
            writes: 0,
        });
    }

    // Force these initial stack values as these should be the values regardless
    // of the memory policy, as it's per 6502 specification..
    vec[0x2FF].value = 0x00;
    vec[0x2FE].value = 0x00;

    vec
}

/// For a given u16 expression, return a tuple formatted like so:
///  .0: The lower byte as u8.
///  .1: Whether the expression is larger than 8 bits.
macro_rules! u16_to_u8_with_carry {
    ($val:expr) => {{
        let low_byte = ($val & 0x00FF) as u8;
        (low_byte, ($val & 0xFF00) != 0)
    }};
}

impl Machine {
    /// Initialize a Machine object by reading the ROM file located at
    /// 'file'. The machine should be initialized to start from the 'start'
    /// address, and the memory should be initialized with the given 'policy'.
    pub fn from(file: &Path, start: u16, policy: MemoryPolicy) -> Result<Self, String> {
        let Ok(mut input) = File::open(file) else {
            return Err(format!(
                "failed to open the given file '{}'",
                file.display()
            ));
        };

        // Read the header in order to detect the PRG ROM size.
        let mut buf = vec![0u8; 0x10];
        if let Err(e) = input.read_exact(&mut buf) {
            match e.kind() {
                ErrorKind::UnexpectedEof => return Err("malformed ROM file".to_string()),
                _ => return Err(e.to_string()),
            }
        }
        let header = match Header::try_from(buf.as_slice()) {
            Ok(h) => h,
            Err(e) => return Err(e.to_string()),
        };

        let mut prg_rom = vec![0u8; header.prg_rom_size * 16 * 1024];
        if let Err(e) = input.read_exact(&mut prg_rom) {
            match e.kind() {
                ErrorKind::UnexpectedEof => {
                    return Err("could not read advertised PRG ROM space".to_string());
                }
                _ => return Err(e.to_string()),
            }
        }

        Ok(Self {
            active: true,
            run_function_mode: false,
            prg_rom,
            prg_rom_size: header.prg_rom_size,
            pc: start as usize,
            skip_pc: false,
            cycles: 7, // NOTE: as per 6502 initialization process.
            extra_cycles: 0,
            page_penalty: 0,
            instructions: 0,
            current_instruction: Instruction {
                identifier: InstructionIdentifier::Start,
                addressing_mode: AddressingMode::Implied,
                cycles: 0,
                opcode: 0,
                size: 0,
                bytes: [0, 0],
            },
            a: 0,
            x: 0,
            y: 0,
            s: 0xFD, // NOTE: as per 6502 initialization process.
            initial_stack_value: 0xFD,
            ram: init_memory(&policy),
            status_register: StatusRegister::default(),
            apu: APU::default(),
            ppu: PPU::default(),
            verbose: false,
            should_report_apu: false,
            should_report_ppu: false,
            policy,
            joypads: [Joypad::default(), Joypad::default()],
        })
    }

    /// Push the given 'inputs' to the controller identified by 'id'.
    ///
    /// NOTE: for now only standard controllers 0 and 1 are supported.
    pub fn push_inputs_to(&mut self, id: usize, inputs: &[u8]) {
        assert!(matches!(id, 0 | 1));

        self.joypads[id].push_inputs(inputs);
    }

    // Report to the standard output the current status of the machine.
    fn report(&mut self) {
        let space = if matches!(
            self.current_instruction.identifier,
            InstructionIdentifier::Unknown
        ) {
            "\t"
        } else if matches!(
            self.current_instruction.addressing_mode,
            AddressingMode::Implied | AddressingMode::RelativeOrZeropage
        ) {
            "\t\t"
        } else {
            "\t"
        };

        let empty = HashMap::new();
        println!(
            "{}{}PC: ${:04X}, cycles: {}, registers: [a: ${:02X}, x: ${:02X}, y: ${:02X}, sp: ${:02X}], status: {}",
            self.current_instruction
                .to_human(self.pc, None, &empty, &empty),
            space,
            self.pc,
            self.cycles,
            self.a,
            self.x,
            self.y,
            self.s,
            self.status_register.humanize(),
        );

        if self.should_report_apu {
            println!(
                "\t\t[APU] DMC ${:X}, Frame counter ${:X}\n",
                self.apu.dmc, self.apu.frame_counter
            );
            self.should_report_apu = false;
        } else if self.should_report_ppu {
            println!(
                "\t\t[PPU] Control: ${:02X}, Mask: ${:02X}, Status: ${:02X}, Scroll: ${:02X}, Address: ${:02X}, Data: ${:02X}, OAM addr: ${:02X}, OAM DMA: ${:02X}\n",
                self.ppu.control,
                self.ppu.mask,
                self.ppu.status,
                self.ppu.scroll,
                self.ppu.address,
                self.ppu.data,
                self.ppu.oam_address,
                self.ppu.oam_dma,
            );
            self.should_report_ppu = false;
        }

        if !self.active {
            println!("<end>");
        }
    }

    // Read the joypad identified by 'id' (0 or 1).
    fn joypad_read(&mut self, id: usize) -> Result<u8, String> {
        assert_matches!(id, 0 | 1);
        let jp = self.joypads.get_mut(id).unwrap();

        match jp.state {
            JoypadState::Waiting | JoypadState::Received => {
                Err("joypad is not ready to send data!".to_string())
            }
            JoypadState::Sending => {
                // Increase the number of reads, and go back to 'Waiting'
                // whenever the whole register has been read.
                jp.reads += 1;
                if jp.reads == 8 {
                    jp.state = JoypadState::Waiting;
                }

                let val = jp.shift & 0x01;
                jp.shift >>= 1;
                Ok(val)
            }
        }
    }

    // Write to the joypad identified by 'id' (0 or 1) with the given 'value'.
    fn joypad_write(&mut self, id: usize, value: u8) -> Result<(), String> {
        assert_matches!(id, 0 | 1);
        let jp = self.joypads.get_mut(id).unwrap();

        match jp.state {
            JoypadState::Waiting => {
                if value != 1 {
                    // NOTE: if we are writing on joypad 2, then there might
                    // be a conflict with the APU frame counter. If that's
                    // the case, then ignore this "error" and just return
                    // early. In any other case, a value != 1 is an error.
                    if id == 0 {
                        return Err(format!("expecting exacly a '1', '{}' received", value));
                    }
                    return Ok(());
                }
                jp.state = JoypadState::Received;
                Ok(())
            }
            JoypadState::Received => {
                if value != 0 {
                    return Err(format!("expecting exacly a '0', '{}' received", value));
                }
                jp.prepare_for_reads()?;
                jp.state = JoypadState::Sending;
                Ok(())
            }
            JoypadState::Sending => {
                Err("writing into a controller while it's sending data".to_string())
            }
        }
    }

    // Tick the PPU after an instruction has been run.
    fn next_ppu(&mut self) -> Result<(), String> {
        self.ppu.status = 0x80;

        Ok(())
    }

    /// Step the execution of the machine by one instruction.
    pub fn next_iteration(&mut self) -> Result<(), String> {
        // Perform a new iteration of the PPU and the CPU.
        self.next_ppu()?;
        self.execute()?;

        // Move the PC automatically unless the current instruction explicitely
        // did so already.
        if self.skip_pc {
            self.skip_pc = false;
        } else {
            self.pc += self.current_instruction.size as usize;
        }

        // Sum up cycles and instructions.
        self.instructions += 1;
        self.cycles += self.current_instruction.cycles as usize;
        if self.extra_cycles > 0 {
            self.cycles += self.extra_cycles;
            self.extra_cycles = 0;
        }

        // At this point we can already send a report of the current status of
        // the machine.
        if self.verbose {
            self.report();
        }

        // After moving the PC, is it out of bounds?
        if self.pc < 0x8000 {
            return Err("out of bounds: program counter is pointing below ROM space".to_string());
        }

        // Fetch the next instruction.
        let address = self.pc - 0x8000;
        let opcode = self.prg_rom.get(address).unwrap();
        self.current_instruction = match OPCODES.get(opcode) {
            Some(instr) => instr.clone(),
            None => {
                return Err(format!(
                    "could not find instruction with opcode <{:02X}>",
                    opcode
                ));
            }
        };

        // Fetch the bytes for the current instruction.
        match self.current_instruction.size {
            2 => {
                self.current_instruction.bytes[0] = *self.prg_rom.get(address + 1).unwrap();
                self.current_instruction.bytes[1] = 0;
            }
            3 => {
                self.current_instruction.bytes[0] = *self.prg_rom.get(address + 1).unwrap();
                self.current_instruction.bytes[1] = *self.prg_rom.get(address + 2).unwrap();
            }
            _ => {
                self.current_instruction.bytes = [0, 0];
            }
        };

        Ok(())
    }

    /// Run until the program counter reaches the given 'address'.
    pub fn until_address(&mut self, address: u16) -> Result<(), String> {
        while self.pc != address as usize && self.active {
            self.next_iteration()?;
        }

        Ok(())
    }

    // Perform a read of the given memory 'address'. Set 'account_read' to true
    // if you want this read to be accounted on the statistics of reads of the
    // given address, false otherwise.
    fn read_memory(&mut self, address: u16, account_read: bool) -> Result<u8, String> {
        let cell = self.ram.get_mut(address as usize).unwrap();

        if !cell.read_allowed {
            return Err(format!(
                "reading was not allowed on address '${:04X}'",
                address
            ));
        }
        if account_read {
            cell.reads += 1;
        }

        Ok(cell.value)
    }

    // Perform a write to the given memory 'address' with the given 'value'.
    fn write_memory(&mut self, address: u16, value: u8) -> Result<(), String> {
        let cell = self.ram.get_mut(address as usize).unwrap();

        if !cell.write_allowed {
            return Err(format!(
                "writing was not allowed on address '${:04X}'",
                address
            ));
        }
        cell.writes += 1;
        cell.value = value;

        Ok(())
    }

    // Print the current status of the stack.
    fn put_stack(&mut self) {
        print!("\t\t[STACK]: ");
        if self.s == 0xFF {
            println!("<empty>");
            return;
        }

        for i in self.s + 1..=0xFF {
            let addr = 0x200 + i as usize;
            print!("{:02X} ", self.ram[addr].value);
        }
        println!();
    }

    // Push the given 'value' to the stack. Set 'report' to true if you want the
    // output from verbose mode.
    fn push_stack(&mut self, value: u8, report: bool) -> Result<(), String> {
        // Write the given value onto the stack.
        let address = 0x200 + self.s as u16;
        self.write_memory(address, value)?;

        // And update the stack pointer if possible.
        self.s -= 1;
        if self.s == self.policy.minimum_stack_value {
            return Err("stack underflow!".to_string());
        }

        if self.verbose && report {
            self.put_stack();
        }

        Ok(())
    }

    // Pop the stack once and return the value that was found. Set 'report' to
    // true if you want the output from verbose mode.
    fn pop_stack(&mut self, account_read: bool, report: bool) -> Result<u8, String> {
        if self.s == self.initial_stack_value {
            return Err("stack overflow!".to_string());
        }

        self.s += 1;

        if self.verbose && report {
            self.put_stack();
        }

        let address = 0x200 + self.s as u16;
        self.read_memory(address, account_read)
    }

    // Push the given address onto the stack.
    fn push_address(&mut self, address: usize) -> Result<(), String> {
        let low = (address as u16 & 0x00FF) as u8;
        let high = ((address as u16 & 0xFF00) >> 8) as u8;

        self.push_stack(high, false)?;
        self.push_stack(low, true)?;

        Ok(())
    }

    // Push the status register onto the stack.
    fn push_status_register(&mut self) -> Result<(), String> {
        // brk flag and bit 5 are always set on 'php'/'brk' as per 6502
        // specification.
        let mut val = 0b00110000;
        self.status_register.brk = true;

        if self.status_register.carry {
            val |= StatusRegister::CARRY;
        }
        if self.status_register.zero {
            val |= StatusRegister::ZERO;
        }
        if self.status_register.interrupt {
            val |= StatusRegister::INTERRUPT;
        }
        if self.status_register.decimal {
            val |= StatusRegister::DECIMAL;
        }
        if self.status_register.overflow {
            val |= StatusRegister::OVERFLOW;
        }
        if self.status_register.negative {
            val |= StatusRegister::NEGATIVE;
        }
        self.push_stack(val, true)?;
        Ok(())
    }

    // Pull the status register from the stack.
    fn pop_status_register(&mut self) -> Result<(), String> {
        let val = self.pop_stack(true, true)?;

        self.status_register.carry = (val & StatusRegister::CARRY) == StatusRegister::CARRY;
        self.status_register.zero = (val & StatusRegister::ZERO) == StatusRegister::ZERO;
        self.status_register.interrupt =
            (val & StatusRegister::INTERRUPT) == StatusRegister::INTERRUPT;
        self.status_register.decimal = (val & StatusRegister::DECIMAL) == StatusRegister::DECIMAL;
        self.status_register.overflow =
            (val & StatusRegister::OVERFLOW) == StatusRegister::OVERFLOW;
        self.status_register.negative =
            (val & StatusRegister::NEGATIVE) == StatusRegister::NEGATIVE;

        // BRK is always cleared. We also clear the break mark now.
        self.status_register.brk = false;
        self.status_register.break_mark = 0;

        Ok(())
    }

    // Returns true of the stack is empty, false otherwise. Note that this
    // just means that the value of the 's' register is the one set as its
    // initial value.
    fn is_stack_empty(&mut self) -> bool {
        self.s == self.initial_stack_value
    }

    // Compare the given 'value' with the one from the current instruction. Then
    // set the proper bits from the status register.
    fn compare(&mut self, value: i16) -> Result<(), String> {
        let memory = self.load()? as i16;
        let res = value - memory;

        // And set flags accordingly.
        self.status_register.zero = res == 0;
        self.status_register.negative = (res as u8 & 0x80) == 0x80;
        self.status_register.carry = (res as u16 & 0xFF00) != 0;

        Ok(())
    }

    /// Execute the current instruction.
    pub fn execute(&mut self) -> Result<(), String> {
        self.status_register.overflow = false;

        match self.current_instruction.identifier {
            // Flag instructions.
            InstructionIdentifier::Sec => self.status_register.carry = true,
            InstructionIdentifier::Clc => self.status_register.carry = false,
            InstructionIdentifier::Sei => self.status_register.interrupt = true,
            InstructionIdentifier::Cli => self.status_register.interrupt = false,
            InstructionIdentifier::Sed => self.status_register.decimal = true,
            InstructionIdentifier::Cld => self.status_register.decimal = false,
            InstructionIdentifier::Clv => self.status_register.overflow = false,

            // Arithmetic and logic.
            InstructionIdentifier::Adc => {
                let mut val = (self.load()? as u16) + self.a as u16;
                if self.status_register.carry {
                    val += 1;
                }
                (self.a, self.status_register.carry) = u16_to_u8_with_carry!(val);

                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::Sbc => {
                let mut val = self.a as i16 - self.load()? as i16;
                if !self.status_register.carry {
                    val -= 1;
                }
                (self.a, self.status_register.carry) = u16_to_u8_with_carry!(val as u16);

                // The carry flag is set as an inverted borrow. Hence, whatever
                // we got from the operation as a "regular 'adc'", then we
                // invert it.
                self.status_register.carry = !self.status_register.carry;

                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::And => {
                let val = self.load()?;
                self.a &= val;
                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::Ora => {
                let val = self.load()?;
                self.a |= val;
                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::Eor => {
                let val = self.load()?;
                self.a ^= val;
                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::Inc => {
                let val = ((self.load()? as u16 + 1) & 0x00FF) as u8;

                self.store(val)?;
                self.status_register.zero = val == 0;
                self.status_register.negative = (val & 0x80) == 0x80;
            }
            InstructionIdentifier::Inx => {
                let val = ((self.x as u16 + 1) & 0x00FF) as u8;

                self.x = val;
                self.status_register.zero = val == 0;
                self.status_register.negative = (val & 0x80) == 0x80;
            }
            InstructionIdentifier::Iny => {
                let val = ((self.y as u16 + 1) & 0x00FF) as u8;

                self.y = val;
                self.status_register.zero = val == 0;
                self.status_register.negative = (val & 0x80) == 0x80;
            }
            InstructionIdentifier::Dec => {
                let mut val = self.load()?;
                if val == 0x00 {
                    self.store(0xFF)?;

                    self.status_register.zero = false;
                    self.status_register.negative = false;
                } else {
                    val -= 1;
                    self.store(val)?;
                    self.status_register.zero = val == 0;
                    self.status_register.negative = (val & 0x80) == 0x80;
                }
            }
            InstructionIdentifier::Dex => {
                if self.x == 0x00 {
                    self.x = 0xFF;

                    self.status_register.zero = false;
                    self.status_register.negative = false;
                } else {
                    self.x -= 1;
                    self.status_register.zero = self.x == 0;
                    self.status_register.negative = (self.x & 0x80) == 0x80;
                }
            }
            InstructionIdentifier::Dey => {
                if self.y == 0x00 {
                    self.y = 0xFF;

                    self.status_register.zero = false;
                    self.status_register.negative = false;
                } else {
                    self.y -= 1;
                    self.status_register.zero = self.y == 0;
                    self.status_register.negative = (self.y & 0x80) == 0x80;
                }
            }
            InstructionIdentifier::Asl => {
                match self.current_instruction.addressing_mode {
                    AddressingMode::Implied => {
                        let val = (self.a as u16) << 1;
                        (self.a, self.status_register.carry) = u16_to_u8_with_carry!(val);
                        self.status_register.zero = self.a == 0;
                        self.status_register.negative = (val & 0x0080) == 0x0080;
                    }
                    _ => {
                        let val = (self.load()? as u16) << 1;
                        self.status_register.carry = (val & 0xFF00) != 0;
                        self.status_register.zero = val == 0;
                        self.store((val & 0x00FF) as u8)?;
                        self.status_register.negative = (val & 0x0080) == 0x0080;
                    }
                };
            }
            InstructionIdentifier::Lsr => {
                match self.current_instruction.addressing_mode {
                    AddressingMode::Implied => {
                        self.status_register.carry = (self.a & 0x1) == 0x1;
                        self.a >>= 1;
                        self.status_register.zero = self.a == 0;
                    }
                    _ => {
                        let mut val = self.load()? as u16;
                        self.status_register.carry = (val & 0x1) == 0x1;
                        val >>= 1;
                        self.status_register.zero = self.a == 0;
                        self.store(val as u8)?;
                    }
                };
                self.status_register.negative = false;
            }
            InstructionIdentifier::Ror => {
                match self.current_instruction.addressing_mode {
                    AddressingMode::Implied => {
                        let carry = self.status_register.carry;
                        self.status_register.carry = (self.a & 0x1) == 0x1;
                        self.a >>= 1;
                        if carry {
                            self.a |= 0x80;
                        }
                        self.status_register.zero = self.a == 0;
                    }
                    _ => {
                        let mut val = self.load()? as usize;
                        let carry = self.status_register.carry;
                        self.status_register.carry = (val & 0x1) == 0x1;
                        val >>= 1;
                        if carry {
                            val |= 0x80;
                        }
                        self.status_register.zero = self.a == 0;
                        self.store(val as u8)?;
                    }
                };
                self.status_register.negative = false;
            }
            InstructionIdentifier::Rol => {
                match self.current_instruction.addressing_mode {
                    AddressingMode::Implied => {
                        let carry = self.status_register.carry;
                        self.status_register.carry = (self.a & 0x80) == 0x80;
                        self.a <<= 1;
                        if carry {
                            self.a |= 0x01;
                        }
                        self.status_register.zero = self.a == 0;
                    }
                    _ => {
                        let mut val = self.load()? as usize;
                        let carry = self.status_register.carry;
                        self.status_register.carry = (val & 0x80) == 0x80;
                        val <<= 1;
                        if carry {
                            val |= 0x01;
                        }
                        self.status_register.zero = self.a == 0;
                        self.store(val as u8)?;
                    }
                };
                self.status_register.negative = false;
            }

            // Compare
            InstructionIdentifier::Cmp => self.compare(self.a as i16)?,
            InstructionIdentifier::Cpx => self.compare(self.x as i16)?,
            InstructionIdentifier::Cpy => self.compare(self.y as i16)?,

            // Load and Store
            InstructionIdentifier::Lda => self.a = self.load()?,
            InstructionIdentifier::Ldx => self.x = self.load()?,
            InstructionIdentifier::Ldy => self.y = self.load()?,
            InstructionIdentifier::Sta => self.store(self.a)?,
            InstructionIdentifier::Stx => self.store(self.x)?,
            InstructionIdentifier::Sty => self.store(self.y)?,

            // Jump and branching.
            InstructionIdentifier::Jsr => {
                let address = self.current_instruction.value();
                if !(0x8000..=0xFFFF).contains(&address) {
                    return Err("invalid jump!".to_string());
                }

                // NOTE: as per 6502 specification, the address should be - 1
                // because the 'rts' instruction will be the one in charge of
                // adding its size to the end PC upon execution. This is kind of
                // pedantic but in the end we record the address being pushed
                // onto the stack and that should be precise.
                let next_address = self.pc + self.current_instruction.size as usize - 1;
                self.push_address(next_address)?;

                self.pc = address;
                self.skip_pc = true;
            }
            InstructionIdentifier::Jmp => {
                let address = self.current_instruction.value();
                if !(0x8000..=0xFFFF).contains(&address) {
                    return Err("invalid jump!".to_string());
                }

                self.pc = address;
                self.skip_pc = true;
            }
            InstructionIdentifier::Bcs => {
                if self.status_register.carry {
                    self.branch();
                }
            }
            InstructionIdentifier::Bcc => {
                if !self.status_register.carry {
                    self.branch();
                }
            }
            InstructionIdentifier::Beq => {
                if self.status_register.zero {
                    self.branch();
                }
            }
            InstructionIdentifier::Bne => {
                if !self.status_register.zero {
                    self.branch();
                }
            }
            InstructionIdentifier::Bpl => {
                if !self.status_register.negative {
                    self.branch();
                }
            }
            InstructionIdentifier::Bmi => {
                if self.status_register.negative {
                    self.branch();
                }
            }
            InstructionIdentifier::Bvc => {
                if !self.status_register.overflow {
                    self.branch();
                }
            }
            InstructionIdentifier::Bvs => {
                if self.status_register.overflow {
                    self.branch();
                }
            }

            InstructionIdentifier::Rts => {
                // If the stack is empty but we were just running a function,
                // then assume that the machine is done.
                if self.is_stack_empty() && self.run_function_mode {
                    if self.active {
                        self.active = false;
                    }
                    return Ok(());
                }

                // Pull the previous address from the stack and jump there.
                //
                // NOTE: as per 6502 specification, the address saved onto the
                // stack was the next instruction before the call, but the size
                // of the 'rts' should also be accounted. Hence the + 1 to the
                // resulting PC.
                let low = self.pop_stack(true, false)? as u16;
                let high = (self.pop_stack(true, true)? as u16) << 8;
                self.pc = (high + low) as usize + 1;
                self.skip_pc = true;
            }

            // Stack
            InstructionIdentifier::Pha => self.push_stack(self.a, true)?,
            InstructionIdentifier::Pla => self.a = self.pop_stack(true, true)?,
            InstructionIdentifier::Php => self.push_status_register()?,
            InstructionIdentifier::Plp => self.pop_status_register()?,

            InstructionIdentifier::Brk => {
                // The next address after a 'brk' is: pc + size of 'brk' (1) +
                // break mark (1).
                let next_address = self.pc + 2;
                self.push_address(next_address)?;
                self.push_status_register()?;

                // The break mark is basically the byte which is in current PC +
                // 1. If that's not possible, then we have a 'brk' as the last
                // instruction with no break mark or something like that, which
                // is just nonsense.
                self.status_register.break_mark = *self
                    .prg_rom
                    .get(self.pc - 0x8000 + 1)
                    .expect("you need to reserve a byte for the break mark");

                // TODO: the next PC is the advertized IRQ one. This one is
                // picked as the last byte from PRG-ROM, but depending on how
                // bank mapping is done, this is not necessarily true.
                let len = self.prg_rom.len();
                let high = &(self.prg_rom[len - 1] as usize) << 8;
                let low = &(self.prg_rom[len - 2] as usize);
                self.pc = high + low;
                self.skip_pc = true;
            }
            InstructionIdentifier::Rti => {
                self.pop_status_register()?;

                let low = self.pop_stack(true, false)? as usize;
                let high = (self.pop_stack(true, true)? as usize) << 8;
                self.pc = high + low;
                self.skip_pc = true;
            }

            // transfer
            InstructionIdentifier::Tax => {
                self.x = self.a;
                self.status_register.zero = self.x == 0;
                self.status_register.negative = (self.x & 0x80) == 0x80;
            }
            InstructionIdentifier::Tay => {
                self.y = self.a;
                self.status_register.zero = self.y == 0;
                self.status_register.negative = (self.y & 0x80) == 0x80;
            }
            InstructionIdentifier::Tsx => {
                self.x = self.s;
                self.status_register.zero = self.x == 0;
                self.status_register.negative = (self.x & 0x80) == 0x80;
            }
            InstructionIdentifier::Txa => {
                self.a = self.x;
                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }
            InstructionIdentifier::Txs => {
                self.s = self.x;
                self.initial_stack_value = self.x;
            }
            InstructionIdentifier::Tya => {
                self.a = self.y;
                self.status_register.zero = self.a == 0;
                self.status_register.negative = (self.a & 0x80) == 0x80;
            }

            // other
            InstructionIdentifier::Bit => {
                let val = self.load()?;
                self.status_register.zero = (val & self.a) == 0;
                self.status_register.negative = (val & 0x80) == 0x80;
                self.status_register.overflow = (val & 0x40) == 0x40;
            }

            InstructionIdentifier::Start | InstructionIdentifier::Nop => {}
            InstructionIdentifier::Unknown => {
                return Err("found an unknown instruction!".to_string());
            }
        }

        Ok(())
    }

    // Perform a branch instruction.
    fn branch(&mut self) {
        let val = self.current_instruction.value() as i8;
        let next = if val > 0 {
            self.pc + val as usize
        } else {
            self.pc - val.wrapping_neg() as usize
        };

        // If the next PC results in crossing the page boundary compared to the
        // old PC, then there is a page penalty to be accounted. That is,
        // branch instructions are defined with a base 'cycles' count, and then
        // needs to be increased either by 1 or 2 depending on whether there was
        // a page penalty or not. We could have easily defined the base 'cycles'
        // with an increased value of 1 and then only increase by one on page
        // penalty, but the definition tries to be close to the 6502
        // specification, which is defined like so. Hence, we keep this quirk.
        if (next & 0xFF00) == (self.pc & 0xFF00) {
            self.extra_cycles += 1;
        } else {
            self.extra_cycles += 2;
            self.page_penalty += 1;
        }
        self.pc = next;
        // TODO
        // self.skip_pc = true;
    }

    // Perform a load instruction and return the read value.
    fn load(&mut self) -> Result<u8, String> {
        let val = self.current_instruction.value();
        let byte = if matches!(
            self.current_instruction.addressing_mode,
            AddressingMode::Immediate
        ) {
            val as u8
        } else {
            let address = self.target_address()?;

            match address {
                0x2000 => self.ppu.control,
                0x2001 => self.ppu.mask,
                0x2002 => {
                    // TODO: also clear the address latch
                    let val = self.ppu.status;
                    self.ppu.status = 0x00;
                    val
                }
                0x2003 => self.ppu.oam_address, // TODO: maybe read fault
                0x2005 => self.ppu.scroll,
                0x2006 => self.ppu.address,
                0x2007 => self.ppu.data,
                0x4010 => self.apu.dmc,
                0x4014 => self.ppu.oam_dma, // TODO: maybe read fault
                0x00..0x2000 => {
                    // NOTE: 0x0800 until 0x2000 are simply mirrors of the first
                    // 2KB. Let's mask out the upper bits.
                    let real = address & 0x07FF;
                    self.read_memory(real as u16, true)?
                }
                0x8000..=0xFFFF => {
                    let real = address - 0x8000;
                    *self.prg_rom.get(real).unwrap()
                }
                0x4016 => self.joypad_read(0)?,
                // NOTE: the joypad 2 and the APU frame counter share the same
                // address, but read is only reserved for joypad 2.
                0x4017 => self.joypad_read(1)?,
                _ => todo!(),
            }
        };

        // Set proper flags from the final value.
        self.status_register.zero = byte == 0;
        self.status_register.negative = (byte & 0x80) == 0x80;

        Ok(byte)
    }

    // Returns the effective address which the current instruction is
    // targetting.
    fn target_address(&mut self) -> Result<usize, String> {
        match self.current_instruction.addressing_mode {
            AddressingMode::Absolute | AddressingMode::RelativeOrZeropage => {
                Ok(self.current_instruction.value())
            }
            AddressingMode::ZeropageIndexedX | AddressingMode::IndexedX => {
                Ok(self.current_instruction.value() + self.x as usize)
            }
            AddressingMode::ZeropageIndexedY | AddressingMode::IndexedY => {
                Ok(self.current_instruction.value() + self.y as usize)
            }
            AddressingMode::IndirectY => {
                let ptr = self.current_instruction.value() as u16;
                let value = self.read_memory(ptr, true)? as u16
                    + ((self.read_memory(ptr + 1, true)? as u16) << 8);
                Ok(value as usize + self.y as usize)
            }
            _ => {
                self.report();
                todo!();
                // Err("bad addressing mode".to_string())
            }
        }
    }

    // Perform a store instruction with the given 'value'.
    fn store(&mut self, value: u8) -> Result<(), String> {
        let address = self.target_address()?;

        match address {
            0x2000 => {
                self.ppu.control = value;
                self.should_report_ppu = self.verbose;
            }
            0x2001 => {
                self.ppu.mask = value;
                self.should_report_ppu = self.verbose;
            }
            0x2002 => {
                self.ppu.status = value;
                self.should_report_ppu = self.verbose;
            }
            0x2003 => {
                self.ppu.oam_address = value;
                self.should_report_ppu = self.verbose;
            }
            0x2005 => {
                self.ppu.scroll = value;
                self.should_report_ppu = self.verbose;
            }
            0x2006 => {
                self.ppu.address = value;
                self.should_report_ppu = self.verbose;
            }
            0x2007 => {
                self.ppu.data = value;
                self.should_report_ppu = self.verbose;
            }
            0x4010 => {
                self.apu.dmc = value;
                self.should_report_apu = self.verbose;
            }
            0x4014 => {
                self.ppu.oam_dma = value;
                self.should_report_ppu = self.verbose;
            }
            0x4016 => self.joypad_write(0, value)?,
            0x4017 => {
                // NOTE: a write on $4017 affects both the APU frame counter and
                // the joypad 2 read sequence.

                self.apu.frame_counter = value;
                self.should_report_apu = self.verbose;

                self.joypad_write(1, value)?;
            }
            0x00..0x2000 => {
                // NOTE: 0x0800 until 0x2000 are simply mirrors of the first
                // 2KB. Let's mask out the upper bits.
                let real = address & 0x07FF;
                self.write_memory(real as u16, value)?;
            }
            _ => {
                self.report();
                todo!()
            }
        };

        Ok(())
    }
}
