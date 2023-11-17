#![forbid(unsafe_code)]

use std::error::Error;
use std::fmt;
use std::fmt::Formatter;
use std::time::Instant;

use opcode::{ConstOp, DecodeError, DisplayOp, FlowOp, MemoryOp, Opcode};

mod opcode;

const FOUR_KB: usize = 0x1000;

pub enum Environment {
    Standard,
    SuperChip,
}

pub struct Display {
    buffer: Vec<Vec<bool>>,
    dimensions: (usize, usize),
}

impl Display {
    const STANDARD_DIMENSIONS: (usize, usize) = (64, 32);
    const SUPER_DIMENSIONS: (usize, usize) = (128, 64);

    const FULL_ON: char = '█';
    const TOP_ON: char = '\u{2580}';
    const BOTTOM_ON: char = '\u{2584}';
    const FULL_OFF: char = ' ';

    pub const RGBA8_BUFF_SIZE: usize = 8192;

    const WHITE_BYTES: [u8; 4] = [u8::MAX, u8::MAX, u8::MAX, u8::MAX];
    const BLACK_BYTES: [u8; 4] = [0, 0, 0, u8::MAX];

    pub fn new(env: Environment) -> Self {
        let dimensions = match env {
            Environment::Standard => Self::STANDARD_DIMENSIONS,
            Environment::SuperChip => Self::SUPER_DIMENSIONS,
        };
        let (width, height) = dimensions;

        let mut buffer = Vec::with_capacity(height);
        for _ in 0..height {
            let mut row_buf = Vec::with_capacity(width);
            for _ in 0..width {
                row_buf.push(false);
            }
            buffer.push(row_buf);
        }

        Self { buffer, dimensions }
    }

    pub fn clear(&mut self) {
        self.buffer.iter_mut()
            .flatten()
            .for_each(|pixel| *pixel = false);
    }

    /// Sets a pixel at the specified coordinate to "on" if `value` is `true` or "off" if `value` is
    /// `false`.
    ///
    /// If the pixel was on then turned off, `true` is returned
    pub fn set_pixel(&mut self, coordinate: (usize, usize), value: bool) -> bool {
        let (x, y) = coordinate;
        let was_on = self.buffer[y][x];
        self.buffer[y][x] = value;

        was_on && !self.buffer[y][x]
    }

    fn get_pixel(&self, coordinate: (usize, usize)) -> bool {
        let (x, y) = coordinate;
        self.buffer[y][x]
    }

    pub fn as_string(&self) -> String {
        let mut display_str = String::new();
        let (width, height) = self.dimensions;

        for row in (0..height).step_by(2) {
            for col in 0..width {
                let top_val = self.get_pixel((col, row));
                let bottom_val = self.get_pixel((col, row + 1));
                let px = if top_val && bottom_val {
                    Display::FULL_ON
                } else if top_val && !bottom_val {
                    Display::TOP_ON
                } else if !top_val && bottom_val {
                    Display::BOTTOM_ON
                } else {
                    Display::FULL_OFF
                };
                display_str.push(px);
            }
            if row < height - 1 {
                display_str.push('\n');
            }
        }

        display_str
    }

    pub fn as_rgba8_buffer(&self) -> Vec<u8> {
        let (width, height) = self.dimensions;
        let mut rgba_buffer = Vec::with_capacity(Display::RGBA8_BUFF_SIZE);

        for y in 0..height {
            for x in 0..width {
                // let buff_idx = (y * width) + x;
                if self.buffer[y][x] {
                    rgba_buffer.extend(Display::WHITE_BYTES.iter());
                } else {
                    rgba_buffer.extend(Display::BLACK_BYTES.iter());
                };
            }
        }

        rgba_buffer
    }
}

#[derive(Debug)]
pub struct ProgramLoadError {
    prg_size: usize,
}

impl ProgramLoadError {
    fn new(prg_size: usize) -> Self {
        Self { prg_size }
    }
}

impl Error for ProgramLoadError {}

impl fmt::Display for ProgramLoadError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "program is too large ({} bytes)", self.prg_size)
    }
}

const REGISTER_COUNT: usize = 16;

pub struct Chip8 {
    /// Program Counter
    pc: usize,
    /// Index Register
    i: usize,
    /// General-purpose variable registers
    v: [u8; REGISTER_COUNT],
    /// Call stack
    stack: Vec<u16>,
    /// Delay timer decremented every 1/60th of a second until it reaches 0
    delay_timer: u8,
    /// Sound timer decremented every 1/60th of a second
    sound_timer: u8,
    /// Addressable memory (4kB)
    memory: [u8; FOUR_KB],
    /// Display instance
    display: Display,
    /// Point in time when previous step completed
    prev_step_instant: Instant,
}

impl Default for Chip8 {
    fn default() -> Self {
        let mut memory = [0u8; FOUR_KB];
        memory[..Chip8::FONT.len()].copy_from_slice(&Chip8::FONT[..]);

        Self {
            pc: Chip8::PRG_START,
            i: 0,
            v: [0u8; REGISTER_COUNT],
            stack: vec![],
            delay_timer: 0,
            sound_timer: 0,
            memory,
            display: Display::new(Environment::Standard),
            prev_step_instant: Instant::now(),
        }
    }
}

impl Chip8 {
    const FLAG_REGISTER: usize = 0xF;

    /// Interpreter font data
    #[rustfmt::skip]
    const FONT: [u8; 80] = [
        0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
        0x20, 0x60, 0x20, 0x20, 0x70, // 1
        0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
        0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
        0x90, 0x90, 0xF0, 0x10, 0x10, // 4
        0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
        0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
        0xF0, 0x10, 0x20, 0x40, 0x40, // 7
        0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
        0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
        0xF0, 0x90, 0xF0, 0x90, 0x90, // A
        0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
        0xF0, 0x80, 0x80, 0x80, 0xF0, // C
        0xE0, 0x90, 0x90, 0x90, 0xE0, // D
        0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
        0xF0, 0x80, 0xF0, 0x80, 0x80  // F
    ];

    /// Program start address
    const PRG_START: usize = 0x0200;
    const MAX_PRG_SIZE: usize = FOUR_KB - (Chip8::PRG_START as usize);

    pub fn load_program(&mut self, prg_data: &[u8]) -> Result<(), ProgramLoadError> {
        let prg_size = prg_data.len();
        if prg_size > Chip8::MAX_PRG_SIZE {
            return Err(ProgramLoadError::new(prg_size));
        }

        let prg_end = (Chip8::PRG_START as usize) + prg_size;
        let write_range = (Chip8::PRG_START as usize)..prg_end;
        self.memory[write_range].copy_from_slice(prg_data);

        // Clear memory past the new program
        if prg_end == self.memory.len() {
            return Ok(());
        }

        for i in prg_end..self.memory.len() {
            self.memory[i] = 0;
        }

        Ok(())
    }

    fn clear_display(&mut self) {
        self.display.clear();
    }

    fn decrement_timers(&mut self) {
        if self.sound_timer > 0 {
            self.sound_timer -= 1;
        }
        if self.delay_timer > 0 {
            self.delay_timer -= 1;
        }
    }

    fn jump(&mut self, address: u16) {
        self.pc = address.into();
    }

    fn set_variable_register(&mut self, index: usize, value: u8) {
        self.v[index] = value;
    }

    fn add_to_register(&mut self, index: usize, value: u8) {
        let result = self.v[index].wrapping_add(value);
        self.v[index] = result;
    }

    fn set_index_register(&mut self, value: u16) {
        self.i = value.into();
    }

    fn draw_sprite(&mut self, coordinate_registers: (usize, usize), height: u8) {
        let x = self.v[coordinate_registers.0];
        let y = self.v[coordinate_registers.1];
        self.v[Chip8::FLAG_REGISTER] = 0;

        for row in 0..height {
            // read byte pointed at by I
            let row_idx = self.i + (row as usize);
            let row_byte = self.memory[row_idx];
            for col in 0..8 {
                let pos_x = (x + col) as usize;
                let pos_y = (y + row) as usize;
                let pixel_val = (row_byte >> (7 - col)) & 0b1;
                let pixel_val = pixel_val == 0b1;

                if self.display.set_pixel((pos_x, pos_y), pixel_val) {
                    self.v[Chip8::FLAG_REGISTER] = 1;
                }
            }
        }
    }

    /// Fetch the next 2-byte instruction from memory and decode it to an Opcode
    fn fetch_and_decode(&mut self) -> Result<Opcode, DecodeError> {
        let pc_local: usize = self.pc.into();
        let high_byte: u16 = self.memory[pc_local].into();
        let low_byte: u16 = self.memory[pc_local + 1].into();
        self.pc += 2;

        let instruction = (high_byte << 8) | low_byte;
        Opcode::decode(instruction)
    }

    fn execute(&mut self, opcode: Opcode) {
        match opcode {
            Opcode::Display(display_op) => match display_op {
                DisplayOp::Clear => {
                    self.clear_display();
                }
                DisplayOp::DrawSprite(coord_registers, height) => {
                    self.draw_sprite(coord_registers, height)
                }
            },
            Opcode::Const(const_op) => match const_op {
                ConstOp::Assign(idx, value) => {
                    self.set_variable_register(idx, value);
                }
                ConstOp::AddAssign(idx, value) => {
                    self.add_to_register(idx, value);
                }
            },
            Opcode::Flow(flow_op) => match flow_op {
                FlowOp::Jump(addr) => {
                    self.jump(addr);
                }
                FlowOp::Return => {
                    todo!();
                }
                FlowOp::Call(_) => {
                    todo!();
                }
                FlowOp::IndexedJump(_) => {
                    todo!();
                }
            },
            Opcode::Memory(memory_op) => match memory_op {
                MemoryOp::SetI(value) => {
                    self.set_index_register(value);
                }
                MemoryOp::AddAssign(_) => {
                    todo!();
                }
                MemoryOp::SetSpriteAddr(_) => {
                    todo!();
                }
                MemoryOp::RegDump(_) => {
                    todo!();
                }
                MemoryOp::RegLoad(_) => {
                    todo!();
                }
            },
            Opcode::Conditional(_) => {
                todo!();
            }
            Opcode::SetVxToVy(_, _) => {
                todo!();
            }
            Opcode::Bitwise(_) => {
                todo!();
            }
            Opcode::Math(_) => {
                todo!();
            }
            Opcode::Random(_, _) => {
                todo!();
            }
            Opcode::Key(_) => {
                todo!();
            }
            Opcode::Timer(_) => {
                todo!();
            }
            Opcode::Bcd(_) => {
                todo!();
            }
        }
    }

    /// Read, decode, and execute a single CPU instruction
    ///
    /// If the word pointed to by PC is not a valid opcode, it will be treated as a no-op, and a
    /// DecodeError will be returned.
    pub fn step(&mut self) -> Result<(), DecodeError> {
        let opcode = self.fetch_and_decode()?;
        self.execute(opcode);
        Ok(())
    }

    pub fn get_display(&self) -> &Display {
        &self.display
    }
}

#[cfg(test)]
mod tests {
    use crate::opcode::DisplayOp;

    use super::*;

    #[test]
    fn display_clear() {
        let mut display = Display::new(Environment::Standard);

        display.buffer.iter_mut()
            .flatten()
            .for_each(|pixel| *pixel = true);

        display.clear();
        assert!(display.buffer.iter().flatten().all(|pixel| !(*pixel)));
    }

    #[test]
    fn chip8_fetch_and_decode() {
        let prg_start: usize = Chip8::PRG_START as usize;
        let mut chip8 = Chip8::default();
        // Clear screen (0x00E0)
        chip8.memory[prg_start] = 0x00;
        chip8.memory[prg_start + 1] = 0xE0;

        let opcode = chip8.fetch_and_decode();
        assert_eq!(opcode, Ok(Opcode::Display(DisplayOp::Clear)));
        assert_eq!(chip8.pc, prg_start + 2);
    }

    #[test]
    fn chip8_add_to_register() {
        let test_data = [(0, 10, 5, 15), (1, u8::MAX, 32, 31)];

        let mut chip8 = Chip8::default();
        for (index, initial, operand, expected) in test_data {
            chip8.v[index] = initial;
            chip8.add_to_register(index, operand);
            assert_eq!(chip8.v[index], expected);
        }
    }

    #[test]
    fn chip8_draw_sprite() {
        let mut chip8 = Chip8::default();
        chip8.v[1] = 10;
        chip8.v[2] = 11;
        chip8.memory[0] = 0b11001010;
        chip8.memory[1] = 0b00110101;

        chip8.draw_sprite((1, 2), 2);
        assert_eq!(chip8.i, 0);
        assert_eq!(chip8.v[0xF], 0);
    }
}
