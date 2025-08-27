use crate::instruction::{Instruction, OPCODES};
use header::Header;
use instruction::InstructionIdentifier;
use std::fs::File;
use std::io::{ErrorKind, Read};
use std::ops::Range;
use xixanta::opcodes::AddressingMode;

pub mod instruction;

// TODO: implement the iterator trait.

// TODO: a lot of things will need to be adapted once we support banking, as
// there needs to be a translation between the Rom's prg_rom and the working
// prg_rom. NOTE: self.ip - 0x8000.

#[derive(Debug)]
pub struct StatusRegister {
    negative: bool,
    overflow: bool,
    brk: bool,
    decimal: bool,
    interrupt: bool,
    zero: bool,
    carry: bool,
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
        }
    }
}

impl StatusRegister {
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

#[derive(Debug, Default)]
pub struct APU {
    pub dmc: u8,
    pub frame_counter: u8,
}

// TODO: memory, plus stores actually do something to it :)
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

// TODO: preserve old values as history, plus context on who modified it.
#[derive(Clone, Copy, Default)]
pub struct MemoryCell {
    pub value: u8,
    pub write_allowed: bool,
    pub read_allowed: bool,
    pub reads: usize,
    pub writes: usize,
}

pub enum MemoryInitialValue {
    Fixed(u8),
    Random,
}

pub struct MemoryPolicy {
    pub initial_value: MemoryInitialValue,
    pub allowed_reads: Vec<Range<usize>>,
    pub allowed_writes: Vec<Range<usize>>,
    pub minimum_stack_value: u8,
}

#[derive(Copy, Clone, Default)]
pub enum JoypadState {
    #[default]
    Waiting,
    Received,
    Sending,
}

#[derive(Copy, Clone, Default)]
pub struct Joypad {
    pub state: JoypadState,
    pub value: u8,
    pub shift: u8,
    reads: u8,
}

impl Joypad {
    fn prepare_for_reads(&mut self) {
        self.value = 0; // TODO: :)
        self.shift = self.value;
        self.reads = 0;
    }
}

pub struct Machine {
    active: bool,
    run_function_mode: bool,

    pub prg_rom: Vec<u8>,
    pub prg_rom_size: usize,
    pub current_instruction: Instruction,

    pub cycles: usize,
    extra_cycles: usize,
    pub instructions: usize,

    pub pc: usize,
    skip_pc: bool,
    pub status_register: StatusRegister,

    pub page_penalty: usize,

    pub ram: Vec<MemoryCell>,

    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub s: u8,

    pub apu: APU,
    pub ppu: PPU,

    pub verbose: bool,
    should_report_apu: bool,
    should_report_ppu: bool,

    initial_stack_value: u8,

    policy: MemoryPolicy,

    joypads: [Joypad; 2],
}

fn init_memory(policy: &MemoryPolicy) -> Vec<MemoryCell> {
    let mut vec = Vec::with_capacity(0x800);

    for i in 0..0x800 {
        let read_allowed = policy.allowed_reads.iter().any(|range| range.contains(&i));
        let write_allowed = policy.allowed_writes.iter().any(|range| range.contains(&i));

        vec.push(MemoryCell {
            value: match policy.initial_value {
                MemoryInitialValue::Fixed(n) => n,
                MemoryInitialValue::Random => todo!(),
            },
            write_allowed,
            read_allowed,
            reads: 0,
            writes: 0,
        });
    }

    vec
}

macro_rules! u16_to_u8_with_carry {
    ($val:expr) => {{
        let low_byte = ($val & 0x00FF) as u8;
        (low_byte, ($val & 0xFF00) != 0)
    }};
}

impl Machine {
    pub fn from(file: &String, start: u16, policy: MemoryPolicy) -> Result<Self, String> {
        let Ok(mut input) = File::open(file) else {
            return Err(format!("failed to open the given file '{}'", file));
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
                    return Err("could not read advertised PRG ROM space".to_string())
                }
                _ => return Err(e.to_string()),
            }
        }

        // TODO: allow for randomized initialization.
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
                affected_on_page: false,
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
            joypads: [Joypad::default(); 2],
        })
    }

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
        println!(
            "{}{}PC: ${:04X}, cycles: {}, registers: [a: ${:02X}, x: ${:02X}, y: ${:02X}, sp: ${:02X}], status: {}",
            self.current_instruction,
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

    fn joypad_read(&mut self, id: usize) -> Result<u8, String> {
        match self.joypads.get_mut(id) {
            Some(jp) => match jp.state {
                JoypadState::Waiting | JoypadState::Received => {
                    Err("joypad is not ready to send data!".to_string())
                }
                JoypadState::Sending => {
                    jp.reads += 1;
                    if jp.reads > 7 {
                        Err("too many reads for the joypad state".to_string())
                    } else {
                        let val = jp.shift & 0x01; // TODO: actually more bits are to be sent
                        jp.shift >>= 1;
                        Ok(val)
                    }
                }
            },
            None => Err("unknown controller!".to_string()),
        }
    }

    fn joypad_write(&mut self, id: usize, value: u8) -> Result<(), String> {
        match self.joypads.get_mut(id) {
            Some(jp) => match jp.state {
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
                    jp.prepare_for_reads();
                    jp.state = JoypadState::Sending;
                    Ok(())
                }
                JoypadState::Sending => {
                    Err("writing into a controller while it's sending data".to_string())
                }
            },
            None => Err("unknown controller!".to_string()),
        }
    }

    // TODO
    fn next_ppu(&mut self) -> Result<(), String> {
        self.ppu.status = 0x80;

        Ok(())
    }

    pub fn next(&mut self) -> Result<(), String> {
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
                ))
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

    pub fn run_function(&mut self) -> Result<(), String> {
        self.run_function_mode = true;

        while self.active {
            self.next()?;
        }

        Ok(())
    }

    // TODO: how to timeout
    pub fn until_address(&mut self, address: u16) -> Result<(), String> {
        while self.pc != address as usize {
            self.next()?;
        }

        Ok(())
    }

    fn read_memory(&mut self, address: u16) -> Result<u8, String> {
        let cell = self.ram.get(address as usize).unwrap();

        if !cell.read_allowed {
            return Err(format!(
                "reading was not allowed on address '${:04X}'",
                address
            ));
        }
        Ok(cell.value)
    }

    fn write_memory(&mut self, address: u16, value: u8) -> Result<(), String> {
        let cell = self.ram.get_mut(address as usize).unwrap();

        if !cell.write_allowed {
            return Err(format!(
                "writing was not allowed on address '${:04X}'",
                address
            ));
        }
        cell.value = value;

        Ok(())
    }

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
        println!("");
    }

    fn push_stack(&mut self, value: u8) -> Result<(), String> {
        // Write the given value onto the stack.
        let address = 0x200 + self.s as u16;
        self.write_memory(address, value)?;

        // And update the stack pointer if possible.
        self.s -= 1;
        if self.s == self.policy.minimum_stack_value {
            return Err("stack underflow!".to_string());
        }

        if self.verbose {
            self.put_stack();
        }

        Ok(())
    }

    fn pop_stack(&mut self) -> Result<u8, String> {
        if self.s == self.initial_stack_value {
            return Err("stack overflow!".to_string());
        }

        self.s += 1;

        if self.verbose {
            self.put_stack();
        }

        let address = 0x200 + self.s as u16;
        self.read_memory(address)
    }

    fn is_stack_empty(&mut self) -> bool {
        self.s == self.initial_stack_value
    }

    fn compare(&mut self, value: i16) -> Result<(), String> {
        let res = value - self.current_instruction.value() as i16;

        self.status_register.zero = res == 0;
        self.status_register.negative = (res as u8 & 0x80) == 0x80;
        self.status_register.carry = (res as u16 & 0xFF00) != 0;

        Ok(())
    }

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
                let address = self.current_instruction.value() as usize;
                if address < 0x8000 || address > 0xFFFF {
                    return Err("invalid jump!".to_string());
                }

                let next_address = self.pc + self.current_instruction.size as usize;
                let low = (next_address as u16 & 0x00FF) as u8;
                let high = ((next_address as u16 & 0xFF00) >> 8) as u8;

                self.push_stack(high)?;
                self.push_stack(low)?;

                self.pc = address;
                self.skip_pc = true;
            }
            InstructionIdentifier::Jmp => {
                let address = self.current_instruction.value() as usize;
                if address < 0x8000 || address > 0xFFFF {
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
            InstructionIdentifier::Rts => {
                // If the stack is empty but we were just running a function,
                // then assume that the machine is done.
                if self.is_stack_empty() && self.run_function_mode {
                    if self.active {
                        self.active = false;
                    }
                    return Ok(());
                }

                // Pull the previous address from the stack and jump there. Note
                // that we have to subtract the current instruction's size
                // because it will be re-added after the call to `execute`.
                let low = self.pop_stack()? as u16;
                let high = (self.pop_stack()? as u16) << 8;
                self.pc = (high + low) as usize;
                self.skip_pc = true;
            }

            // transfer
            InstructionIdentifier::Tax => self.x = self.a,
            InstructionIdentifier::Tay => self.y = self.a,
            InstructionIdentifier::Tsx => self.x = self.s,
            InstructionIdentifier::Txa => self.a = self.x,
            InstructionIdentifier::Txs => {
                self.s = self.x;
                self.initial_stack_value = self.x;
            }
            InstructionIdentifier::Tya => self.a = self.y,

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

    fn branch(&mut self) {
        let val = self.current_instruction.value() as i8;
        let next = if val > 0 {
            self.pc + val as usize
        } else {
            self.pc - val.wrapping_neg() as usize
        };

        if (next & 0xFF00) == (self.pc & 0xFF00) {
            self.extra_cycles += 2;
            self.page_penalty += 1;
        } else {
            self.extra_cycles += 1;
        }
        self.pc = next;
        // TODO
        // self.skip_pc = true;
    }

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
                    self.read_memory(real as u16)?
                }
                0x8000..=0xFFFF => {
                    let real = address - 0x8000;
                    *self.prg_rom.get(real).unwrap() as u8
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
                let value =
                    self.read_memory(ptr)? as u16 + ((self.read_memory(ptr + 1)? as u16) << 8);
                Ok(value as usize + self.y as usize)
            }
            _ => {
                self.report();
                todo!();
                // Err("bad addressing mode".to_string())
            }
        }
    }

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
                println!("HERE!: {:04X}", address);
                self.report();
                todo!()
            }
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO
}
