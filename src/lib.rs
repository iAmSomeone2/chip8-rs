#![forbid(unsafe_code)]

extern crate core;

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

    const PX_ON: char = '█';
    const PX_OFF: char = ' ';

    pub fn new(env: Environment) -> Self {
        let dimensions = match env {
            Environment::Standard => Self::STANDARD_DIMENSIONS,
            Environment::SuperChip => Self::SUPER_DIMENSIONS
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

        Self {
            buffer,
            dimensions,
        }
    }

    /// Sets a pixel at the specified coordinate to "on" if `value` is `true` or "off" if `value` is
    /// `false`.
    ///
    /// If the pixel was on then turned off, then `true` is returned
    pub fn set_pixel(&mut self, coordinate: (usize, usize), value: bool) -> bool {
        let (x, y) = coordinate;
        let was_on = self.buffer[y][x];
        self.buffer[y][x] = value;

        return was_on && !self.buffer[y][x];
    }

    fn get_pixel(&self, coordinate: (usize, usize)) -> bool {
        let (x, y) = coordinate;
        return self.buffer[y][x];
    }

    pub fn as_string(&self) -> String {
        let mut display_str = String::new();
        let (width, height) = self.dimensions;

        for row in 0..height {
            for col in 0..width {
                let px = if self.get_pixel((col, row)) { Display::PX_ON } else { Display::PX_OFF };
                display_str.push(px);
            }
            if row < height-1 {
                display_str.push('\n');
            }
        }

        return display_str;
    }
}


pub struct Chip8 {
    /// Program Counter
    pc: u16,
    /// Index Register
    i: u16,
    /// General-purpose variable registers
    v: [u8; 16],
    /// Call stack
    stack: Vec<u16>,
    /// Delay timer decremented every 1/60th of a second until it reaches 0
    delay_timer: u8,
    /// Sound timer decremented every 1/60th of a second
    sound_timer: u8,
    /// Addressable memory (4kB)
    memory: [u8; FOUR_KB],
}

impl Chip8 {
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
    const PRG_START: u16 = 0x0200;

    pub fn new() -> Self {
        let mut memory = [0u8; FOUR_KB];
        for i in 0..Chip8::FONT.len() {
            memory[i] = Chip8::FONT[i];
        }

        Self {
            pc: Chip8::PRG_START,
            i: 0,
            v: [0u8; 16],
            stack: vec![],
            delay_timer: 0,
            sound_timer: 0,
            memory,
        }
    }

    /// Fetch the next 2-byte instruction from memory and return it as a u16
    fn fetch(&mut self) -> u16 {
        let pc_local: usize = self.pc.into();
        let high_byte: u16 = self.memory[pc_local].into();
        let low_byte: u16 = self.memory[pc_local + 1].into();
        self.pc += 2;

        (high_byte << 8) | low_byte
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chip8_fetch() {
        let prg_start: usize = Chip8::PRG_START as usize;
        let mut chip8 = Chip8::new();
        chip8.memory[prg_start] = 0x12;
        chip8.memory[prg_start + 1] = 0x34;

        let opcode = chip8.fetch();
        assert_eq!(opcode, 0x1234);
        assert_eq!(chip8.pc, (prg_start + 2) as u16);
    }
}
