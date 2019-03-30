use std::time::{Instant, Duration};
use crate::mmu::Mmu;
use crate::instruction::{Instruction, OpCode, OpVal, OpBit, OpCond};
use crate::instruction::OpCode::PUSH;

// How fast our CPU clocks at, 4.19 MHz standard for DMG
const CYCLES_PER_SECOND: f64 = 419000.0;
// How many nano seconds required for 1 cycle
const NANOS_PER_CYCLE: u32 = (1_000_000_000.0 / CYCLES_PER_SECOND) as u32;

pub struct Cpu {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: u8,
    h: u8,
    l: u8,
    sp: u16,
    pc: u16,
    // represent as a duration of cycles available so we dont deal with imprecision
    cycles_duration: Duration,
    // last instant at which we updated our cycles count
    cycles_updated: Instant,
}

impl Cpu {
    // Create a new CPU with initialized registers and cycle timing.
    pub fn new() -> Cpu {
        /*Cpu {
            a: 0x01,
            b: 0x00,
            c: 0x13,
            d: 0x00,
            e: 0xD8,
            f: 0xB0,
            sp: 0xFFFE,
            pc: 0x0100,
            cycles_duration: Duration::new(0, 0),
            cycles_updated: Instant::now(),
        }*/
        Cpu {
            a: 0x00,
            b: 0x00,
            c: 0x00,
            d: 0x00,
            e: 0x00,
            f: 0x00,
            h: 0x00,
            l: 0x00,
            sp: 0x0000,
            pc: 0x0000,
            cycles_duration: Duration::new(0, 0),
            cycles_updated: Instant::now(),
        }
    }

    // Updates the CPU with the correct amount of cycles, given it's speed and elapsed time.
    pub fn update_cycles(&mut self, now: Instant) {
        let elapsed = now.duration_since(self.cycles_updated);

        self.cycles_duration += elapsed;
        self.cycles_updated = now;
    }

    // Execute the next instruction and return true if it was executed.
    //
    // False if we did not have enough cycles to execute the next instruction.
    pub fn next_instruction(&mut self, mmu: &mut Mmu) -> bool {
        let instruction = mmu.fetch_instruction(self.pc);

        return self.execute_instruction(mmu, instruction);
    }

    // Handle interrupts that may have occurred.
    pub fn handle_interrupts() {}

    // Attempt to execute the given instruction.
    //
    // Returns true if an instruction was executed, false if we did not have enough cycles.
    fn execute_instruction(&mut self, mmu: &mut Mmu, instruction: Instruction) -> bool {
        // TODO: Do this after checking for branch condition...good enough for now though
        let cycles_received = self.request_cycles(instruction.cycles(true));
        if !cycles_received {
            return false;
        }

        let mut increment_pc = true;
        // Execute instruction operation...just enough for the boot rom for now
        match instruction.opcode() {
            OpCode::BIT(bit, target) => {
                let set = self.test_bit(mmu, bit, target);
                self.set_flag_register(Some(!set), Some(false),
                                       Some(true), None);
            }
            OpCode::INC(value) => {
                self.increment_reg(mmu, value);
            }
            OpCode::JR(cond, OpVal::D8(offset)) => {
                if self.test_condition(cond) {
                    // offset is signed, if we cast straight from u8 to u16 the
                    // signed bit will not be promoted to the resulting u16 value
                    let signed_offset = offset as i8;

                    self.pc = self.pc.wrapping_add(signed_offset as u16);
                    increment_pc = false;
                }
            }
            OpCode::LDL(target, value) => {
                self.load_left(mmu, target, value)
            }
            OpCode::LDRD(value, target) => {
                self.load_left(mmu, target, value);
                self.decrement_addr(target);
            }
            OpCode::LDRN(value, target) => {
                let real_target = match target {
                    OpVal::C => OpVal::A16(self.c as u16 + 0xFF00),
                    OpVal::D8(d) => OpVal::A16(d as u16 + 0xFF00),
                    illegal => panic!("illegal LDRN with target {:?}", illegal)
                };

                self.load_left(mmu, real_target, value);
            }
            OpCode::XOR(OpVal::A) => {
                self.a ^= self.a;
                self.set_flag_register(Some(self.a == 0), Some(false),
                                       Some(false), Some(false));
            }
            opcode => panic!("unimplemented op code {:?} at offset 0x{:X?}", opcode, instruction.offset())
        }

        // Increment program counter based on instruction size
        if increment_pc {
            self.pc += instruction.size();
        }

        return true;
    }

    fn increment_reg(&mut self, mmu: &mut Mmu, value: OpVal) {
        let mut opt_flag_update = None;

        match value {
            OpVal::A => {
                let old_a = self.a;
                let new_a = self.a.wrapping_add(1);
                self.a = new_a;

                opt_flag_update = Some((old_a, new_a));
            }
            OpVal::B => {
                
            }
        }
    }

    fn test_condition(&self, condition: Option<OpCond>) -> bool {
        match condition {
            Some(OpCond::NZ) => !self.is_zero_flag_set(),
            Some(OpCond::Z) => self.is_zero_flag_set(),
            Some(OpCond::NC) => !self.is_carry_flag_set(),
            Some(OpCond::C) => self.is_carry_flag_set(),
            None => true
        }
    }

    fn test_bit(&self, mmu: &Mmu, bit: OpBit, value: OpVal) -> bool {
        fn bit_checker(bit: OpBit, value: u8) -> bool {
            match bit {
                OpBit::B0 => value & 0x01 == 0x01,
                OpBit::B1 => value & 0x02 == 0x02,
                OpBit::B2 => value & 0x04 == 0x04,
                OpBit::B3 => value & 0x08 == 0x08,
                OpBit::B4 => value & 0x10 == 0x10,
                OpBit::B5 => value & 0x20 == 0x20,
                OpBit::B6 => value & 0x40 == 0x40,
                OpBit::B7 => value & 0x80 == 0x80
            }
        }

        match value {
            OpVal::A => bit_checker(bit, self.a),
            OpVal::B => bit_checker(bit, self.b),
            OpVal::C => bit_checker(bit, self.c),
            OpVal::D => bit_checker(bit, self.d),
            OpVal::E => bit_checker(bit, self.e),
            OpVal::H => bit_checker(bit, self.h),
            OpVal::L => bit_checker(bit, self.l),
            OpVal::AHL => bit_checker(bit, mmu.fetch_value(u16::from_be_bytes([self.h, self.l]))),
            illegal => panic!("illegal value {:?} in bit test", value)
        }
    }

    // TODO: Figure out semantics for underflow on gameboy
    // Decrement a value that is used as an address (not just any 16 bit value)
    fn decrement_addr(&mut self, target: OpVal) {
        match target {
            OpVal::ABC => {
                let addr = u16::from_be_bytes([self.b, self.c]) - 1;
                let result = addr.to_be_bytes();
                self.b = result[0];
                self.c = result[1];
            }
            OpVal::ADE => {
                let addr = u16::from_be_bytes([self.d, self.e]) - 1;
                let result = addr.to_be_bytes();
                self.d = result[0];
                self.e = result[1];
            }
            OpVal::AHL => {
                let addr = u16::from_be_bytes([self.h, self.l]) - 1;
                let result = addr.to_be_bytes();
                self.h = result[0];
                self.l = result[1];
            }
            _ => panic!("illegal target {:?} in address decrement")
        }
    }

    // Load the given value into the given target (load left).
    // TODO: Some of these loads may be illegal, should spot check them later
    fn load_left(&mut self, mmu: &mut Mmu, target: OpVal, value: OpVal) {
        // So this doesn't explode in variants, pick the target first, then call another function
        // to get the value with the return value being our expected size, otherwise, blow up...
        match target {
            OpVal::A => self.a = self.load_u8(mmu, value),
            OpVal::B => self.b = self.load_u8(mmu, value),
            OpVal::C => self.c = self.load_u8(mmu, value),
            OpVal::D => self.d = self.load_u8(mmu, value),
            OpVal::E => self.e = self.load_u8(mmu, value),
            OpVal::H => self.h = self.load_u8(mmu, value),
            OpVal::L => self.l = self.load_u8(mmu, value),
            OpVal::ABC => {
                let result = self.load_u8(mmu, value);
                mmu.store_value(u16::from_be_bytes([self.b, self.c]), result);
            }
            OpVal::ADE => {
                let result = self.load_u8(mmu, value);
                mmu.store_value(u16::from_be_bytes([self.d, self.e]), result);
            }
            OpVal::AHL => {
                let result = self.load_u8(mmu, value);
                mmu.store_value(u16::from_be_bytes([self.h, self.l]), result);
            }
            OpVal::AF => {
                let result = self.load_u16(value).to_be_bytes();
                self.a = result[0];
                self.f = result[1];
            }
            OpVal::BC => {
                let result = self.load_u16(value).to_be_bytes();
                self.b = result[0];
                self.c = result[1];
            }
            OpVal::DE => {
                let result = self.load_u16(value).to_be_bytes();
                self.d = result[0];
                self.e = result[1];
            }
            OpVal::HL => {
                let result = self.load_u16(value).to_be_bytes();
                self.h = result[0];
                self.l = result[1];
            }
            OpVal::SP => self.sp = self.load_u16(value),
            OpVal::A16(addr) => mmu.store_value(addr, self.load_u8(mmu, value)),
            illegal => panic!("illegal target {:?} in load", illegal)
        }
    }

    // Load a u8 from the given value.
    //
    // If the value is not a u8, we panic.
    fn load_u8(&self, mmu: &Mmu, value: OpVal) -> u8 {
        // Only looking at potential u8 loads here, any u16 loads will panic
        match value {
            OpVal::A => self.a,
            OpVal::B => self.b,
            OpVal::C => self.c,
            OpVal::D => self.d,
            OpVal::E => self.e,
            OpVal::H => self.h,
            OpVal::L => self.l,
            OpVal::D8(d) => d,
            OpVal::ABC => mmu.fetch_value(u16::from_be_bytes([self.b, self.c])),
            OpVal::ADE => mmu.fetch_value(u16::from_be_bytes([self.d, self.e])),
            OpVal::AHL => mmu.fetch_value(u16::from_be_bytes([self.h, self.l])),
            OpVal::A16(a) => mmu.fetch_value(a),
            illegal => panic!("illegal operand {:?} in 8 bit load", illegal)
        }
    }

    // Load a u16 from the given value.
    //
    // If the value is not a u16, we panic.
    fn load_u16(&self, value: OpVal) -> u16 {
        match value {
            OpVal::AF => u16::from_be_bytes([self.a, self.f]),
            OpVal::BC => u16::from_be_bytes([self.b, self.c]),
            OpVal::DE => u16::from_be_bytes([self.d, self.e]),
            OpVal::HL => u16::from_be_bytes([self.h, self.l]),
            OpVal::D16(d) => d,
            illegal => panic!("illegal operand {:?} in 16 bit load", illegal)
        }
    }

    fn is_zero_flag_set(&self) -> bool {
        self.f & 0x80 == 0x80
    }

    fn is_sub_flag_set(&self) -> bool {
        self.f & 0x40 == 0x40
    }

    fn is_half_carry_flag_set(&self) -> bool {
        self.f & 0x20 == 0x20
    }

    fn is_carry_flag_set(&self) -> bool {
        self.f & 0x10 == 0x10
    }

    // Update the flag register with the given value.
    //
    // If None is given, the flag will not be touched.
    fn set_flag_register(&mut self, zero: Option<bool>, sub: Option<bool>,
                         half_carry: Option<bool>, carry: Option<bool>) {
        // TODO: Condense this...
        match zero {
            Some(true) => self.f |= 0x80,
            Some(false) => self.f &= 0x7F,
            None => ()
        }

        match sub {
            Some(true) => self.f |= 0x40,
            Some(false) => self.f &= 0xBF,
            None => ()
        }

        match half_carry {
            Some(true) => self.f |= 0x20,
            Some(false) => self.f &= 0xDF,
            None => ()
        }

        match carry {
            Some(true) => self.f |= 0x10,
            Some(false) => self.f &= 0xEF,
            None => ()
        }
    }

    // Request the given number of cycles in order to execute some operation.
    fn request_cycles(&mut self, cycles: u8) -> bool {
        let nanos_required = NANOS_PER_CYCLE * cycles as u32;
        let duration_required = Duration::new(0, nanos_required);

        match self.cycles_duration.checked_sub(duration_required) {
            Some(updated_cycles) => {
                self.cycles_duration = updated_cycles;
                true
            }
            None => false
        }
    }
}