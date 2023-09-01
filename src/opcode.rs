use std::{error::Error, fmt};
use std::convert::identity;
use std::fmt::Formatter;
use crate::opcode::BitOp::{AndAssign, OrAssign, ShiftLeft, ShiftRight, XorAssign};
use crate::opcode::ConstOp::{AddAssign, Assign};
use crate::opcode::KeyOp::{GetKey, KeyEq, KeyNe};
use crate::opcode::MemoryOp::SetI;
use crate::opcode::Opcode::{Bcd, Conditional, Display, Flow, Key, Memory, Random, SetVxToVy, Timer};
use crate::opcode::TimerOp::{GetDelay, SetDelay, SetSound};

/// Error used when a 16-bit word cannot be decoded into an opcode
#[derive(Debug, PartialEq, Eq)]
pub struct DecodeError {
    word: u16,
}

impl DecodeError {
    pub fn new(word: u16) -> Self {
        Self {
            word
        }
    }
}

impl fmt::Display for DecodeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Unrecognized opcode {:06x}", self.word)
    }
}

impl Error for DecodeError {}


/// Split a u16 value into its 4 nibbles
fn split_nibbles(word: u16) -> (u8, u8, u8, u8) {
    let nibble0 = ((word & 0xF000) >> 12) as u8;
    let nibble1 = ((word & 0x0F00) >> 8) as u8;
    let nibble2 = ((word & 0x00F0) >> 4) as u8;
    let nibble3 = (word & 0x000F) as u8;

    return (nibble0, nibble1, nibble2, nibble3);
}

/// Split a u16 value into its first nibble and a 12-bit constant
fn split_12bit(word: u16) -> (u8, u16) {
    let nibble = ((word & 0xF000) >> 12) as u8;
    let constant = word & 0x0FFF;

    return (nibble, constant);
}

fn split_8bit(word: u16) -> (u8, u8, u8) {
    let nibble0 = ((word & 0xF000) >> 12) as u8;
    let nibble1 = ((word & 0x0F00) >> 8) as u8;
    let byte = (word & 0x00FF) as u8;

    return (nibble0, nibble1, byte);
}

/// Operations performable on the display
#[derive(Debug, PartialEq, Eq)]
enum DisplayOp {
    /// Clear the display
    Clear,
    /// Draw a sprite at coordinate (VX, VY) 8 pixels wide with a height of N
    DrawSprite((usize, usize), u8),
}

impl DisplayOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        return match word {
            0x00E0 => {
                Ok(Self::Clear)
            }
            _ => {
                let nibbles = split_nibbles(word);
                if nibbles.0 != 0xD {
                    return Err(DecodeError::new(word));
                }

                Ok(
                    DisplayOp::DrawSprite(
                        (nibbles.1 as usize, nibbles.2 as usize), nibbles.3,
                    )
                )
            }
        };
    }
}

/// Program flow operations
#[derive(Debug, PartialEq, Eq)]
enum FlowOp {
    /// Return from subroutine
    Return,
    /// Jump to contained address
    Jump(u16),
    /// Call subroutine at contained address
    Call(u16),
    /// Jump to the value of the contained address indexed by V0
    IndexedJump(u16),
}

impl FlowOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        return match word {
            0x00EE => Ok(FlowOp::Return),
            _ => {
                let (nibble, constant) = split_12bit(word);
                match nibble {
                    0x1 => Ok(FlowOp::Jump(constant)),
                    0x2 => Ok(FlowOp::Call(constant)),
                    0xB => Ok(FlowOp::IndexedJump(constant)),
                    _ => Err(DecodeError::new(word))
                }
            }
        }
    }
}

/// Conditional flow operations
#[derive(Debug, PartialEq, Eq)]
enum ConditionalOp {
    /// Skip the next instruction if Vx == NN
    VxEqNN(usize, u8),
    /// Skip the next instruction if Vx != NN
    VxNeNN(usize, u8),
    /// Skip the next instruction if Vx == Vy
    VxEqVy(usize, usize),
    /// Skip the next instruction if Vx != Vy
    VxNeVy(usize, usize),
}

impl ConditionalOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let nibbles = split_nibbles(word);
        return match nibbles.0 {
            0x3 | 0x4 => {
                let (_, idx_x, value) = split_8bit(word);
                let idx_x = idx_x as usize;
                match nibbles.0 {
                    0x3 => Ok(ConditionalOp::VxEqNN(idx_x, value)),
                    0x4 => Ok(ConditionalOp::VxNeNN(idx_x, value)),
                    _ => Err(DecodeError::new(word))
                }
            },
            0x5 | 0x9 => {
                let (_, idx_x, idx_y, check) = nibbles;
                if check != 0 {
                    return Err(DecodeError::new(word));
                }
                let idx_x = idx_x as usize;
                let idx_y = idx_y as usize;
                match nibbles.0 {
                    0x5 => Ok(ConditionalOp::VxEqVy(idx_x, idx_y)),
                    0x9 => Ok(ConditionalOp::VxNeVy(idx_x, idx_y)),
                    _ => Err(DecodeError::new(word))
                }
            }
            _ => Err(DecodeError::new(word))
        }
    }
}

/// Constant value operations
#[derive(Debug, PartialEq, Eq)]
enum ConstOp {
    /// Set Vx to NN
    Assign(usize, u8),
    /// Add the value of NN to Vx
    AddAssign(usize, u8),
}

impl ConstOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (nibble, idx_x, value) = split_8bit(word);
        let idx_x = idx_x as usize;
        return match nibble {
            0x6 => Ok(Assign(idx_x, value)),
            0x7 => Ok(AddAssign(idx_x, value)),
            _ => Err(DecodeError::new(word)),
        }
    }
}

/// Bitwise operations
#[derive(Debug, PartialEq, Eq)]
enum BitOp {
    /// Set Vx to Vx OR Vy (Vx |= Vy)
    OrAssign(usize, usize),
    /// Set Vx to Vx AND Vy (Vx &= Vy)
    AndAssign(usize, usize),
    /// Set Vx to Vx XOR Vy (Vx ^= Vy)
    XorAssign(usize, usize),
    /// Store the least-significant bit of Vx in VF and shift Vx to the right by 1
    ShiftRight(usize),
    /// Store the most-significant bit of Vx in VF and shift Vx to the left by 1
    ShiftLeft(usize),
}

impl BitOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (check_nib, idx_x, idx_y, n) = split_nibbles(word);
        if check_nib != 0x8 {
            return Err(DecodeError::new(word));
        }
        let idx_x = idx_x as usize;
        let idx_y = idx_y as usize;

        return match n {
            0x1 => Ok(OrAssign(idx_x, idx_y)),
            0x2 => Ok(AndAssign(idx_x, idx_y)),
            0x3 => Ok(XorAssign(idx_x, idx_y)),
            0x6 => Ok(ShiftRight(idx_x)),
            0xE => Ok(ShiftLeft(idx_x)),
            _ => Err(DecodeError::new(word))
        }
    }
}

/// Math operations
#[derive(Debug, PartialEq, Eq)]
enum MathOp {
    /// Add Vy to Vx. Set VF to 1 if there is a carry (overflow); otherwise 0
    AddAssign(usize, usize),
    /// Subtract Vy from Vx. Set VF to 0 if there is a borrow (underflow); otherwise 1
    SubAssign(usize, usize),
    /// Set Vx to Vy minus Vx. Set VF to 0 if there is a borrow (underflow); otherwise 1
    Subtract(usize, usize),
}

impl MathOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (check_nib, idx_x, idx_y, n) = split_nibbles(word);
        if check_nib != 0x8 {
            return Err(DecodeError::new(word));
        }
        let idx_x = idx_x as usize;
        let idx_y = idx_y as usize;

        return match n {
            0x4 => Ok(MathOp::AddAssign(idx_x, idx_y)),
            0x5 => Ok(MathOp::SubAssign(idx_x, idx_y)),
            0x7 => Ok(MathOp::Subtract(idx_x, idx_y)),
            _ => Err(DecodeError::new(word))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum MemoryOp {
    /// Set I to the address NNN
    SetI(u16),
    /// Add Vx to I
    AddAssign(usize),
    /// Set I to the location of the sprite for the character in Vx
    SetSpriteAddr(usize),
    /// Store from V0 to Vx in memory starting at address I
    RegDump(usize),
    /// Load into V0 through Vx with values from memory starting at address I
    RegLoad(usize),
}

impl MemoryOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (nibble, value) = split_12bit(word);
        if nibble != 0xA && nibble != 0xF {
            return Err(DecodeError::new(word));
        }

        return match nibble {
            0xA => Ok(SetI(value)),
            0xF => {
                let idx_x = ((value & 0x0F00) >> 8) as usize;
                let sub_op = value & 0x00FF;
                match sub_op {
                    0x1E => Ok(MemoryOp::AddAssign(idx_x)),
                    0x29 => Ok(MemoryOp::SetSpriteAddr(idx_x)),
                    0x55 => Ok(MemoryOp::RegDump(idx_x)),
                    0x65 => Ok(MemoryOp::RegLoad(idx_x)),
                    _ => Err(DecodeError::new(word))
                }
            }
            _ => Err(DecodeError::new(word))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum KeyOp {
    /// Skip the next instruction if the key stored in Vx is pressed
    KeyEq(usize),
    /// Skip the next instruction if the key stored in Vx is not pressed
    KeyNe(usize),
    /// Awaits a keypress and stores the value in Vx (halting)
    GetKey(usize),
}

impl KeyOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (nibble, idx_x, sub_op) = split_8bit(word);
        let idx_x = idx_x as usize;
        return match nibble {
            0xE => {
                match sub_op {
                    0x9E => Ok(KeyEq(idx_x)),
                    0xA1 => Ok(KeyNe(idx_x)),
                    _ => Err(DecodeError::new(word))
                }
            }
            0xF => {
                match sub_op {
                    0x0A => Ok(GetKey(idx_x)),
                    _ => Err(DecodeError::new(word))
                }
            }
            _ => Err(DecodeError::new(word))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum TimerOp {
    /// Set Vx to the value of the delay timer
    GetDelay(usize),
    /// Set the delay timer to Vx
    SetDelay(usize),
    /// Set the sound timer to Vx
    SetSound(usize),
}

impl TimerOp {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let (check_nib, idx_x, sub_op) = split_8bit(word);
        if check_nib != 0xF {
            return Err(DecodeError::new(word));
        }

        let idx_x = idx_x as usize;
        return match sub_op {
            0x07 => Ok(GetDelay(idx_x)),
            0x15 => Ok(SetDelay(idx_x)),
            0x18 => Ok(SetSound(idx_x)),
            _ => Err(DecodeError::new(word))
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Opcode {
    /// Display operations (see [DisplayOp])
    Display(DisplayOp),
    /// Program flow operations (see [FlowOp])
    Flow(FlowOp),
    /// Conditional flow operations (see [ConditionalOp])
    Conditional(ConditionalOp),
    /// Assign constant values to registers (see [ConstOp])
    Const(ConstOp),
    /// Assign the value of Vx to the value of Vy
    SetVxToVy(usize, usize),
    /// Bitwise operations (see [BitOp])
    Bitwise(BitOp),
    /// Math operations (see [MathOp])
    Math(MathOp),
    /// Memory operations (see [MemoryOp])
    Memory(MemoryOp),
    /// Set Vx to the result of a random number AND NN
    Random(usize, u8),
    /// Keyboard operations
    Key(KeyOp),
    /// Timer manipulation operations
    Timer(TimerOp),
    /// Store the BCD representation of Vx at the memory location pointed at by I
    Bcd(usize),
}

impl Opcode {
    fn decode(word: u16) -> Result<Self, DecodeError> {
        let nibble = split_nibbles(word);
        return match word {
            0x00E0 => Ok(Display(DisplayOp::Clear)),
            0x00EE => Ok(Flow(FlowOp::Return)),
            _ => {
                match nibble.0 {
                    0x1 | 0x2 | 0xB => {
                        // Flow operations
                        let op = FlowOp::decode(word)?;
                        Ok(Flow(op))
                    },
                    0x3 | 0x4 | 0x5 | 0x9 => {
                        // Conditional operations
                        let op = ConditionalOp::decode(word)?;
                        Ok(Conditional(op))
                    }
                    0xD => {
                        // Draw sprite
                        let (_, idx_x, idx_y, n) = nibble;
                        Ok(Display(DisplayOp::DrawSprite((idx_x as usize, idx_y as usize), n)))
                    }
                    0x6 | 0x7 => {
                        // Const operations
                        let op = ConstOp::decode(word)?;
                        Ok(Self::Const(op))
                    }
                    0x8 => {
                        match nibble.3 {
                            0x0 => Ok(SetVxToVy(nibble.1 as usize, nibble.2 as usize)),
                            0x1 | 0x2 | 0x3 | 0x6 | 0xE => {
                                // Bitwise operations
                                let op = BitOp::decode(word)?;
                                Ok(Self::Bitwise(op))
                            },
                            0x4 | 0x5 | 0x7 => {
                                // Math operations
                                let op = MathOp::decode(word)?;
                                Ok(Self::Math(op))
                            }
                            _ => Err(DecodeError::new(word))
                        }
                    }
                    0xA => {
                        // Memory operations 1/2
                        let op = MemoryOp::decode(word)?;
                        Ok(Memory(op))
                    }
                    0xC => {
                        // Random
                        let (_, idx_x, value) = split_8bit(word);
                        let idx_x = idx_x as usize;
                        Ok(Random(idx_x, value))
                    }
                    0xE => {
                        // Key operations 1/2
                        let op = KeyOp::decode(word)?;
                        Ok(Key(op))
                    }
                    0xF => {
                        let sub_op = word & 0x00FF;
                        match sub_op {
                            0x07 | 0x15 | 0x18 => {
                                // Timer operations
                                let op = TimerOp::decode(word)?;
                                Ok(Timer(op))
                            }
                            0x0A => {
                                // Key operations 2/2
                                let op = KeyOp::decode(word)?;
                                Ok(Key(op))
                            }
                            0x1E | 0x29 | 0x55 | 0x65 => {
                                // Memory operation 2/2
                                let op = MemoryOp::decode(word)?;
                                Ok(Memory(op))
                            }
                            0x33 => {
                                // BCD operation
                                let idx_x = ((word & 0x0F00) >> 8) as usize;
                                Ok(Bcd(idx_x))
                            }
                            _ => Err(DecodeError::new(word))
                        }
                    }
                    _ => Err(DecodeError::new(word))
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::opcode::Opcode::Timer;
    use super::*;

    #[test]
    fn split_nibbles() {
        let input = 0x1234;
        let expected = (0x1, 0x2, 0x3, 0x4);

        assert_eq!(super::split_nibbles(input), expected);
    }

    #[test]
    fn split_12bit() {
        let input = 0x1234;
        let expected = (0x1, 0x0234);

        assert_eq!(super::split_12bit(input), expected);
    }

    #[test]
    fn split_8bit() {
        let input = 0x1234;
        let expected = (0x1, 0x2, 0x34);

        assert_eq!(super::split_8bit(input), expected);
    }

    const ERR_OP: u16 = 0x0012;

    #[test]
    fn decode_display_op() {
        let test_data = [
            (0x00E0, Ok(DisplayOp::Clear)),
            (0xD123, Ok(DisplayOp::DrawSprite((1, 2), 3))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(DisplayOp::decode(input), result);
        }
    }

    #[test]
    fn decode_flow_op() {
        let test_data = [
            (0x00EE, Ok(FlowOp::Return)),
            (0x1ABC, Ok(FlowOp::Jump(0x0ABC))),
            (0x2ABC, Ok(FlowOp::Call(0x0ABC))),
            (0xBABC, Ok(FlowOp::IndexedJump(0x0ABC))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(FlowOp::decode(input), result);
        }
    }

    #[test]
    fn decode_conditional_op() {
        let test_data = [
            (0x3110, Ok(ConditionalOp::VxEqNN(1, 0x10))),
            (0x4220, Ok(ConditionalOp::VxNeNN(2, 0x20))),
            (0x5340, Ok(ConditionalOp::VxEqVy(0x3, 0x4))),
            (0x5343, Err(DecodeError::new(0x5343))),
            (0x9340, Ok(ConditionalOp::VxNeVy(0x3, 0x4))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(ConditionalOp::decode(input), result);
        }
    }

    #[test]
    fn decode_const_op() {
        let test_data = [
            (0x6822, Ok(Assign(0x8, 0x22))),
            (0x7344, Ok(AddAssign(0x3, 0x44))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(ConstOp::decode(input), result);
        }
    }

    #[test]
    fn decode_bit_op() {
        let test_data = [
            (0x8121, Ok(OrAssign(1, 2))),
            (0x8342, Ok(AndAssign(3, 4))),
            (0x8213, Ok(XorAssign(2, 1))),
            (0x8AB6, Ok(ShiftRight(0xA))),
            (0x8BAE, Ok(ShiftLeft(0xB))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(BitOp::decode(input), result);
        }
    }

    #[test]
    fn decode_math_op() {
        let test_data = [
            (0x8AB4, Ok(MathOp::AddAssign(0xA, 0xB))),
            (0x8CD5, Ok(MathOp::SubAssign(0xC, 0xD))),
            (0x8EF7, Ok(MathOp::Subtract(0xE, 0xF))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(MathOp::decode(input), result);
        }
    }

    #[test]
    fn decode_memory_op() {
        let test_data = [
            (0xA123, Ok(MemoryOp::SetI(0x0123))),
            (0xFA1E, Ok(MemoryOp::AddAssign(0xA))),
            (0xFB29, Ok(MemoryOp::SetSpriteAddr(0xB))),
            (0xF355, Ok(MemoryOp::RegDump(0x3))),
            (0xFC65, Ok(MemoryOp::RegLoad(0xC))),
            (0xF099, Err(DecodeError::new(0xF099))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(MemoryOp::decode(input), result);
        }
    }

    #[test]
    fn decode_key_op() {
        let test_data = [
            (0xEA9E, Ok(KeyOp::KeyEq(0xA))),
            (0xECA1, Ok(KeyOp::KeyNe(0xC))),
            (0xE000, Err(DecodeError::new(0xE000))),
            (0xF00A, Ok(KeyOp::GetKey(0x0))),
            (0xF000, Err(DecodeError::new(0xF000))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(KeyOp::decode(input), result);
        }
    }

    #[test]
    fn decode_timer_op() {
        let test_data = [
            (0xFA07, Ok(GetDelay(0xA))),
            (0xFB15, Ok(SetDelay(0xB))),
            (0xFC18, Ok(SetSound(0xC))),
            (0xF000, Err(DecodeError::new(0xF000))),
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(TimerOp::decode(input), result);
        }
    }

    #[test]
    fn decode_opcode() {
        let test_data = [
            // Flow
            (0x00EE, Ok(Flow(FlowOp::Return))),
            (0x1ABC, Ok(Flow(FlowOp::Jump(0x0ABC)))),
            // Display
            (0x00E0, Ok(Display(DisplayOp::Clear))),
            (0xD123, Ok(Display(DisplayOp::DrawSprite((1, 2), 3)))),
            // Conditional
            (0x4220, Ok(Conditional(ConditionalOp::VxNeNN(2, 0x20)))),
            (0x5343, Err(DecodeError::new(0x5343))),
            // Const
            (0x7344, Ok(Opcode::Const(AddAssign(0x3, 0x44)))),
            // Bitwise
            (0x8213, Ok(Opcode::Bitwise(XorAssign(2, 1)))),
            // Math
            (0x8AB4, Ok(Opcode::Math(MathOp::AddAssign(0xA, 0xB)))),
            // Memory
            (0xA123, Ok(Memory(SetI(0x0123)))),
            (0xFB29, Ok(Memory(MemoryOp::SetSpriteAddr(0xB)))),
            // Key
            (0xECA1, Ok(Key(KeyNe(0xC)))),
            (0xF00A, Ok(Key(GetKey(0x0)))),
            // Timer
            (0xFB15, Ok(Timer(SetDelay(0xB)))),
            (0xFC18, Ok(Timer(SetSound(0xC)))),
            // Random
            (0xCABC, Ok(Random(0xA, 0xBC))),
            // BCD
            (0xFA33, Ok(Bcd(0xA))),
            // Error
            (ERR_OP, Err(DecodeError::new(ERR_OP)))
        ];

        for data in test_data {
            let (input, result) = data;
            assert_eq!(Opcode::decode(input), result);
        }
    }
}
