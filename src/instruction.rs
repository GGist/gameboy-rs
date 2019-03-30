#[derive(Debug, Copy, Clone)]
pub enum OpVal {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8(u8),
    AF,
    // value at address bc
    ABC,
    BC,
    // value at address de
    ADE,
    DE,
    // value at address hl
    AHL,
    HL,
    SP,
    PC,
    D16(u16),
    // value at address nn
    A16(u16)
}

#[derive(Debug, Copy, Clone)]
pub enum OpCond {
    NZ,
    Z,
    NC,
    C
}

#[derive(Debug, Copy, Clone)]
pub enum OpBit {
    B0,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7
}

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    ADC(OpVal),
    ADD(OpVal, OpVal),
    AND(OpVal),
    BIT(OpBit, OpVal),
    CALL(Option<OpCond>, OpVal),
    CCF,
    CP(OpVal),
    CPL,
    DAA,
    DEC(OpVal),
    DI,
    EI,
    INC(OpVal),
    JP(Option<OpCond>, OpVal),
    JR(Option<OpCond>, OpVal),
    HALT,
    // load left
    LDL(OpVal, OpVal),
    // load left with right operand + FF00 (ex: LD A,(C) -> LDLN(A, C) ->  load C + FF00 into A)
    LDLN(OpVal, OpVal),
    // load right with right operand + FF00 (ex: LD (C),A -> LDRN(A, C) -> load A into C + FF00)
    LDRN(OpVal, OpVal),
    // load left with right operand increment (ex: LD A,(HLI) -> LDLI(A, HLI) -> load addr HL into A, increment HL)
    LDLI(OpVal, OpVal),
    // load right with right operand increment (ex: LD (HLI),A -> LDRI(A, HLI) -> load A into addr HL, increment HL)
    LDRI(OpVal, OpVal),
    // see LDLI but decrement
    LDLD(OpVal, OpVal),
    // see LDRI but decrement
    LDRD(OpVal, OpVal),
    // load SP + n into HL
    LDHL(OpVal),
    NOP,
    OR(OpVal),
    POP(OpVal),
    PUSH(OpVal),
    RES(OpBit, OpVal),
    RET(Option<OpCond>),
    RETI,
    RL(OpVal),
    RLC(OpVal),
    RR(OpVal),
    RRC(OpVal),
    RST(OpVal),
    SBC(OpVal),
    SCF,
    SET(OpBit, OpVal),
    SLA(OpVal),
    SRA(OpVal),
    SRL(OpVal),
    // ???
    STOP,
    SUB(OpVal),
    SWAP(OpVal),
    XOR(OpVal)
}

#[derive(Debug, Copy, Clone)]
pub struct Instruction {
    code: OpCode,
    normal_cycles: u8,
    branch_cycles: u8,
    instr_size: u16,
    // TODO: this is actually after the instruction...
    offset: usize
}

impl Instruction {
    // Decode a single instruction give a slab of bytes and an offset at which the instruction exists.
    pub fn decode(bytes: &[u8], mut offset: usize) -> Result<Instruction, String> {
        decode(bytes, &mut offset)
    }

    // Decode all instructions in the given slab of bytes.
    pub fn decode_all(bytes: &[u8]) -> Result<Vec<Instruction>, String> {
        let mut instructions = Vec::new();
        let mut offset = 0;

        while bytes.len() > offset {
            let instruction = decode(&bytes, &mut offset)?;

            instructions.push(instruction);
        }

        return Ok(instructions)
    }

    // Get the size of the current instruction.
    pub fn size(&self) -> u16 {
        self.instr_size
    }

    // Get the number of cycles that the current instruction uses.
    //
    // If the instruction takes the branch, set branched to true.
    pub fn cycles(&self, branched: bool) -> u8 {
        if branched {
            self.branch_cycles
        } else {
            self.normal_cycles
        }
    }

    // Get the underlying op code for the given instruction.
    pub fn opcode(&self) -> OpCode {
        self.code
    }

    pub fn offset(&self) -> usize {
        self.offset
    }
}

// TODO: Add cycle count to return values, some instructions have different cycles depending on branch
// Decode the instruction identified by the code using the given instructions buffer.
//
// http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
fn decode(bytes: &[u8], offset: &mut usize) -> Result<Instruction, String> {
    let code = get_u8(bytes, offset)?;

    match code {
        0x00 => map(OpCode::NOP, 4, 4, *offset),
        0x01 => map_u16(bytes, 12, 12, offset, |direct| {
            OpCode::LDL(OpVal::BC, OpVal::D16(direct))
        }),
        0x02 => map(OpCode::LDL(OpVal::ABC, OpVal::A), 8, 8, *offset),
        0x03 => map(OpCode::INC(OpVal::BC), 8, 8, *offset),
        0x04 => map(OpCode::INC(OpVal::B), 4, 4, *offset),
        0x05 => map(OpCode::DEC(OpVal::B), 4, 4, *offset),
        0x06 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::B, OpVal::D8(direct))
        }),
        0x07 => map(OpCode::RLC(OpVal::A), 4, 4, *offset),
        0x08 => map_u16(bytes, 20, 20, offset, |direct| {
            OpCode::LDL(OpVal::SP, OpVal::D16(direct))
        }),
        0x09 => map(OpCode::ADD(OpVal::HL, OpVal::BC), 8, 8, *offset),
        0x0A => map(OpCode::LDL(OpVal::A, OpVal::ABC), 8, 8, *offset),
        0x0B => map(OpCode::DEC(OpVal::BC), 8, 8, *offset),
        0x0C => map(OpCode::INC(OpVal::C), 4, 4, *offset),
        0x0D => map(OpCode::DEC(OpVal::C), 4, 4, *offset),
        0x0E => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::C, OpVal::D8(direct))
        }),
        0x0F => map(OpCode::RRC(OpVal::A), 4, 4, *offset),
        // TODO: Make sure this is correct, assuming 0 dead bytes
        0x10 => map(OpCode::STOP, 4, 4, *offset),
        0x11 => map_u16(bytes, 12, 12, offset, |direct| {
            OpCode::LDL(OpVal::DE, OpVal::D16(direct))
        }),
        0x12 => map(OpCode::LDL(OpVal::ADE, OpVal::A), 8, 8, *offset),
        0x13 => map(OpCode::INC(OpVal::DE), 8, 8, *offset),
        0x14 => map(OpCode::INC(OpVal::D), 4, 4, *offset),
        0x15 => map(OpCode::DEC(OpVal::D), 4, 4, *offset),
        0x16 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::D, OpVal::D8(direct))
        }),
        0x17 => map(OpCode::RL(OpVal::A), 4, 4, *offset),
        0x18 => map_u8(bytes, 12, 12, offset, |direct| {
            OpCode::JR(Option::None, OpVal::D8(direct))
        }),
        0x19 => map(OpCode::ADD(OpVal::HL, OpVal::DE), 8, 8, *offset),
        0x1A => map(OpCode::LDL(OpVal::A, OpVal::ADE), 8, 8, *offset),
        0x1B => map(OpCode::DEC(OpVal::DE), 8, 8, *offset),
        0x1C => map(OpCode::INC(OpVal::E), 4, 4, *offset),
        0x1D => map(OpCode::DEC(OpVal::E), 4, 4, *offset),
        0x1E => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::E, OpVal::D8(direct))
        }),
        0x1F => map(OpCode::RR(OpVal::A), 4, 4, *offset),
        0x20 => map_u8(bytes, 8, 12, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::NZ), OpVal::D8(direct))
        }),
        0x21 => map_u16(bytes, 12, 12, offset, |direct| {
            OpCode::LDL(OpVal::HL, OpVal::D16(direct))
        }),
        0x22 => map(OpCode::LDRI(OpVal::A, OpVal::AHL), 8, 8, *offset),
        0x23 => map(OpCode::INC(OpVal::HL), 8, 8, *offset),
        0x24 => map(OpCode::INC(OpVal::H), 4, 4, *offset),
        0x25 => map(OpCode::DEC(OpVal::H), 4, 4, *offset),
        0x26 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::H, OpVal::D8(direct))
        }),
        0x27 => map(OpCode::DAA, 4, 4, *offset),
        0x28 => map_u8(bytes, 8, 12, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::Z), OpVal::D8(direct))
        }),
        0x29 => map(OpCode::ADD(OpVal::HL, OpVal::HL), 8, 8, *offset),
        0x2A => map(OpCode::LDLI(OpVal::A, OpVal::AHL), 8, 8, *offset),
        0x2B => map(OpCode::DEC(OpVal::HL), 8, 8, *offset),
        0x2C => map(OpCode::INC(OpVal::L), 4, 4, *offset),
        0x2D => map(OpCode::DEC(OpVal::L), 4, 4, *offset),
        0x2E => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::L, OpVal::D8(direct))
        }),
        0x2F => map(OpCode::CPL, 4, 4, *offset),
        0x30 => map_u8(bytes, 8, 12, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::NC), OpVal::D8(direct))
        }),
        0x31 => map_u16(bytes, 12, 12, offset, |direct| {
            OpCode::LDL(OpVal::SP, OpVal::D16(direct))
        }),
        0x32 => map(OpCode::LDRD(OpVal::A, OpVal::AHL), 8, 8, *offset),
        0x33 => map(OpCode::INC(OpVal::SP), 8, 8, *offset),
        0x34 => map(OpCode::INC(OpVal::AHL), 12, 12, *offset),
        0x35 => map(OpCode::DEC(OpVal::AHL), 12, 12, *offset),
        0x36 => map_u8(bytes, 12, 12, offset, |direct| {
            OpCode::LDL(OpVal::AHL, OpVal::D8(direct))
        }),
        0x37 => map(OpCode::SCF, 4, 4, *offset),
        0x38 => map_u8(bytes, 8, 12, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::C), OpVal::D8(direct))
        }),
        0x39 => map(OpCode::ADD(OpVal::HL, OpVal::SP), 8, 8, *offset),
        0x3A => map(OpCode::LDLD(OpVal::A, OpVal::HL), 8, 8, *offset),
        0x3B => map(OpCode::DEC(OpVal::SP), 8, 8, *offset),
        0x3C => map(OpCode::INC(OpVal::A), 4, 4, *offset),
        0x3D => map(OpCode::DEC(OpVal::A), 4, 4, *offset),
        0x3E => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::LDL(OpVal::A, OpVal::D8(direct))
        }),
        0x3F => map(OpCode::CCF, 4, 4, *offset),
        0x40 => map(OpCode::LDL(OpVal::B, OpVal::B), 4, 4, *offset),
        0x41 => map(OpCode::LDL(OpVal::B, OpVal::C), 4, 4, *offset),
        0x42 => map(OpCode::LDL(OpVal::B, OpVal::D), 4, 4, *offset),
        0x43 => map(OpCode::LDL(OpVal::B, OpVal::E), 4, 4, *offset),
        0x44 => map(OpCode::LDL(OpVal::B, OpVal::H), 4, 4, *offset),
        0x45 => map(OpCode::LDL(OpVal::B, OpVal::L), 4, 4, *offset),
        0x46 => map(OpCode::LDL(OpVal::B, OpVal::AHL), 8, 8, *offset),
        0x47 => map(OpCode::LDL(OpVal::B, OpVal::A), 4, 4, *offset),
        0x48 => map(OpCode::LDL(OpVal::C, OpVal::B), 4, 4, *offset),
        0x49 => map(OpCode::LDL(OpVal::C, OpVal::C), 4, 4, *offset),
        0x4A => map(OpCode::LDL(OpVal::C, OpVal::D), 4, 4, *offset),
        0x4B => map(OpCode::LDL(OpVal::C, OpVal::E), 4, 4, *offset),
        0x4C => map(OpCode::LDL(OpVal::C, OpVal::H), 4, 4, *offset),
        0x4D => map(OpCode::LDL(OpVal::C, OpVal::L), 4, 4, *offset),
        0x4E => map(OpCode::LDL(OpVal::C, OpVal::AHL), 8, 8, *offset),
        0x4F => map(OpCode::LDL(OpVal::C, OpVal::A), 4, 4, *offset),
        0x50 => map(OpCode::LDL(OpVal::D, OpVal::B), 4, 4, *offset),
        0x51 => map(OpCode::LDL(OpVal::D, OpVal::C), 4, 4, *offset),
        0x52 => map(OpCode::LDL(OpVal::D, OpVal::D), 4, 4, *offset),
        0x53 => map(OpCode::LDL(OpVal::D, OpVal::E), 4, 4, *offset),
        0x54 => map(OpCode::LDL(OpVal::D, OpVal::H), 4, 4, *offset),
        0x55 => map(OpCode::LDL(OpVal::D, OpVal::L), 4, 4, *offset),
        0x56 => map(OpCode::LDL(OpVal::D, OpVal::AHL), 8, 8, *offset),
        0x57 => map(OpCode::LDL(OpVal::D, OpVal::A), 4, 4, *offset),
        0x58 => map(OpCode::LDL(OpVal::E, OpVal::B), 4, 4, *offset),
        0x59 => map(OpCode::LDL(OpVal::E, OpVal::C), 4, 4, *offset),
        0x5A => map(OpCode::LDL(OpVal::E, OpVal::D), 4, 4, *offset),
        0x5B => map(OpCode::LDL(OpVal::E, OpVal::E), 4, 4, *offset),
        0x5C => map(OpCode::LDL(OpVal::E, OpVal::H), 4, 4, *offset),
        0x5D => map(OpCode::LDL(OpVal::E, OpVal::L), 4, 4, *offset),
        0x5E => map(OpCode::LDL(OpVal::E, OpVal::AHL), 8, 8, *offset),
        0x5F => map(OpCode::LDL(OpVal::E, OpVal::A), 4, 4, *offset),
        0x60 => map(OpCode::LDL(OpVal::H, OpVal::B), 4, 4, *offset),
        0x61 => map(OpCode::LDL(OpVal::H, OpVal::C), 4, 4, *offset),
        0x62 => map(OpCode::LDL(OpVal::H, OpVal::D), 4, 4, *offset),
        0x63 => map(OpCode::LDL(OpVal::H, OpVal::E), 4, 4, *offset),
        0x64 => map(OpCode::LDL(OpVal::H, OpVal::H), 4, 4, *offset),
        0x65 => map(OpCode::LDL(OpVal::H, OpVal::L), 4, 4, *offset),
        0x66 => map(OpCode::LDL(OpVal::H, OpVal::AHL), 8, 8, *offset),
        0x67 => map(OpCode::LDL(OpVal::H, OpVal::A), 4, 4, *offset),
        0x68 => map(OpCode::LDL(OpVal::L, OpVal::B), 4, 4, *offset),
        0x69 => map(OpCode::LDL(OpVal::L, OpVal::C), 4, 4, *offset),
        0x6A => map(OpCode::LDL(OpVal::L, OpVal::D), 4, 4, *offset),
        0x6B => map(OpCode::LDL(OpVal::L, OpVal::E), 4, 4, *offset),
        0x6C => map(OpCode::LDL(OpVal::L, OpVal::H), 4, 4, *offset),
        0x6D => map(OpCode::LDL(OpVal::L, OpVal::L), 4, 4, *offset),
        0x6E => map(OpCode::LDL(OpVal::L, OpVal::AHL), 8, 8, *offset),
        0x6F => map(OpCode::LDL(OpVal::L, OpVal::A), 4, 4, *offset),
        0x70 => map(OpCode::LDL(OpVal::AHL, OpVal::B), 8, 8, *offset),
        0x71 => map(OpCode::LDL(OpVal::AHL, OpVal::C), 8, 8, *offset),
        0x72 => map(OpCode::LDL(OpVal::AHL, OpVal::D), 8, 8, *offset),
        0x73 => map(OpCode::LDL(OpVal::AHL, OpVal::E), 8, 8, *offset),
        0x74 => map(OpCode::LDL(OpVal::AHL, OpVal::H), 8, 8, *offset),
        0x75 => map(OpCode::LDL(OpVal::AHL, OpVal::L), 8, 8, *offset),
        0x76 => map(OpCode::HALT, 4, 4, *offset),
        0x77 => map(OpCode::LDL(OpVal::AHL, OpVal::A), 8, 8, *offset),
        0x78 => map(OpCode::LDL(OpVal::A, OpVal::B), 4, 4, *offset),
        0x79 => map(OpCode::LDL(OpVal::A, OpVal::C), 4, 4, *offset),
        0x7A => map(OpCode::LDL(OpVal::A, OpVal::D), 4, 4, *offset),
        0x7B => map(OpCode::LDL(OpVal::A, OpVal::E), 4, 4, *offset),
        0x7C => map(OpCode::LDL(OpVal::A, OpVal::H), 4, 4, *offset),
        0x7D => map(OpCode::LDL(OpVal::A, OpVal::L), 4, 4, *offset),
        0x7E => map(OpCode::LDL(OpVal::A, OpVal::AHL), 8, 8, *offset),
        0x7F => map(OpCode::LDL(OpVal::A, OpVal::A), 4, 4, *offset),
        0x80 => map(OpCode::ADD(OpVal::A, OpVal::B), 4, 4, *offset),
        0x81 => map(OpCode::ADD(OpVal::A, OpVal::C), 4, 4, *offset),
        0x82 => map(OpCode::ADD(OpVal::A, OpVal::D), 4, 4, *offset),
        0x83 => map(OpCode::ADD(OpVal::A, OpVal::E), 4, 4, *offset),
        0x84 => map(OpCode::ADD(OpVal::A, OpVal::H), 4, 4, *offset),
        0x85 => map(OpCode::ADD(OpVal::A, OpVal::L), 4, 4, *offset),
        0x86 => map(OpCode::ADD(OpVal::A, OpVal::AHL), 8, 8, *offset),
        0x87 => map(OpCode::ADD(OpVal::A, OpVal::A), 4, 4, *offset),
        0x88 => map(OpCode::ADC(OpVal::B), 4, 4, *offset),
        0x89 => map(OpCode::ADC(OpVal::C), 4, 4, *offset),
        0x8A => map(OpCode::ADC(OpVal::D), 4, 4, *offset),
        0x8B => map(OpCode::ADC(OpVal::E), 4, 4, *offset),
        0x8C => map(OpCode::ADC(OpVal::H), 4, 4, *offset),
        0x8D => map(OpCode::ADC(OpVal::L), 4, 4, *offset),
        0x8E => map(OpCode::ADC(OpVal::AHL), 8, 8, *offset),
        0x8F => map(OpCode::ADC(OpVal::A), 4, 4, *offset),
        0x90 => map(OpCode::SUB(OpVal::B), 4, 4, *offset),
        0x91 => map(OpCode::SUB(OpVal::C), 4, 4, *offset),
        0x92 => map(OpCode::SUB(OpVal::D), 4, 4, *offset),
        0x93 => map(OpCode::SUB(OpVal::E), 4, 4, *offset),
        0x94 => map(OpCode::SUB(OpVal::H), 4, 4, *offset),
        0x95 => map(OpCode::SUB(OpVal::L), 4, 4, *offset),
        0x96 => map(OpCode::SUB(OpVal::AHL), 8, 8, *offset),
        0x97 => map(OpCode::SUB(OpVal::A), 4, 4, *offset),
        0x98 => map(OpCode::SBC(OpVal::B), 4, 4, *offset),
        0x99 => map(OpCode::SBC(OpVal::C), 4, 4, *offset),
        0x9A => map(OpCode::SBC(OpVal::D), 4, 4, *offset),
        0x9B => map(OpCode::SBC(OpVal::E), 4, 4, *offset),
        0x9C => map(OpCode::SBC(OpVal::H), 4, 4, *offset),
        0x9D => map(OpCode::SBC(OpVal::L), 4, 4, *offset),
        0x9E => map(OpCode::SBC(OpVal::AHL), 8, 8, *offset),
        0x9F => map(OpCode::SBC(OpVal::A), 4, 4, *offset),
        0xA0 => map(OpCode::AND(OpVal::B), 4, 4, *offset),
        0xA1 => map(OpCode::AND(OpVal::C), 4, 4, *offset),
        0xA2 => map(OpCode::AND(OpVal::D), 4, 4, *offset),
        0xA3 => map(OpCode::AND(OpVal::E), 4, 4, *offset),
        0xA4 => map(OpCode::AND(OpVal::H), 4, 4, *offset),
        0xA5 => map(OpCode::AND(OpVal::L), 4, 4, *offset),
        0xA6 => map(OpCode::AND(OpVal::AHL), 8, 8, *offset),
        0xA7 => map(OpCode::AND(OpVal::A), 4, 4, *offset),
        0xA8 => map(OpCode::XOR(OpVal::B), 4, 4, *offset),
        0xA9 => map(OpCode::XOR(OpVal::C), 4, 4, *offset),
        0xAA => map(OpCode::XOR(OpVal::D), 4, 4, *offset),
        0xAB => map(OpCode::XOR(OpVal::E), 4, 4, *offset),
        0xAC => map(OpCode::XOR(OpVal::H), 4, 4, *offset),
        0xAD => map(OpCode::XOR(OpVal::L), 4, 4, *offset),
        0xAE => map(OpCode::XOR(OpVal::AHL), 8, 8, *offset),
        0xAF => map(OpCode::XOR(OpVal::A), 4, 4, *offset),
        0xB0 => map(OpCode::OR(OpVal::B), 4, 4, *offset),
        0xB1 => map(OpCode::OR(OpVal::C), 4, 4, *offset),
        0xB2 => map(OpCode::OR(OpVal::D), 4, 4, *offset),
        0xB3 => map(OpCode::OR(OpVal::E), 4, 4, *offset),
        0xB4 => map(OpCode::OR(OpVal::H), 4, 4, *offset),
        0xB5 => map(OpCode::OR(OpVal::L), 4, 4, *offset),
        0xB6 => map(OpCode::OR(OpVal::AHL), 8, 8, *offset),
        0xB7 => map(OpCode::OR(OpVal::A), 4, 4, *offset),
        0xB8 => map(OpCode::CP(OpVal::B), 4, 4, *offset),
        0xB9 => map(OpCode::CP(OpVal::C), 4, 4, *offset),
        0xBA => map(OpCode::CP(OpVal::D), 4, 4, *offset),
        0xBB => map(OpCode::CP(OpVal::E), 4, 4, *offset),
        0xBC => map(OpCode::CP(OpVal::H), 4, 4, *offset),
        0xBD => map(OpCode::CP(OpVal::L), 4, 4, *offset),
        0xBE => map(OpCode::CP(OpVal::AHL), 8, 8, *offset),
        0xBF => map(OpCode::CP(OpVal::A), 4, 4, *offset),
        0xC0 => map(OpCode::RET(Some(OpCond::NZ)), 8, 20, *offset),
        0xC1 => map(OpCode::POP(OpVal::BC), 12, 12, *offset),
        0xC2 => map_u16(bytes, 12, 16, offset, |direct| {
            OpCode::JP(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xC3 => map_u16(bytes, 16, 16, offset, |direct| {
            OpCode::JP(None, OpVal::D16(direct))
        }),
        0xC4 => map_u16(bytes, 12, 24, offset, |direct| {
            OpCode::CALL(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xC5 => map(OpCode::PUSH(OpVal::BC), 16, 16, *offset),
        0xC6 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::ADD(OpVal::A, OpVal::D8(direct))
        }),
        0xC7 => map(OpCode::RST(OpVal::D8(0x00)), 16, 16, *offset),
        0xC8 => map(OpCode::RET(Some(OpCond::Z)), 8, 20, *offset),
        0xC9 => map(OpCode::RET(None), 16, 16, *offset),
        0xCA => map_u16(bytes, 12, 16, offset, |direct| {
            OpCode::JP(Some(OpCond::Z), OpVal::D16(direct))
        }),
        0xCB => decode_cb(bytes, offset),
        0xCC => map_u16(bytes, 12, 24, offset, |direct| {
            OpCode::CALL(Some(OpCond::Z), OpVal::D16(direct))
        }),
        0xCD => map_u16(bytes, 24, 24, offset, |direct| {
            OpCode::CALL(None, OpVal::D16(direct))
        }),
        0xCE => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::ADC(OpVal::D8(direct))
        }),
        0xCF => map(OpCode::RST(OpVal::D8(0x08)), 16, 16, *offset),
        0xD0 => map(OpCode::RET(Some(OpCond::NC)), 8, 20, *offset),
        0xD1 => map(OpCode::POP(OpVal::DE), 12, 12, *offset),
        0xD2 => map_u16(bytes, 12, 16, offset, |direct| {
            OpCode::JP(Some(OpCond::NC), OpVal::D16(direct))
        }),
        0xD4 => map_u16(bytes, 12, 24, offset, |direct| {
            OpCode::CALL(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xD5 => map(OpCode::PUSH(OpVal::DE), 16, 16, *offset),
        0xD6 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::SUB(OpVal::D8(direct))
        }),
        0xD7 => map(OpCode::RST(OpVal::D8(0x10)), 16, 16, *offset),
        0xD8 => map(OpCode::RET(Some(OpCond::C)), 8, 20, *offset),
        0xD9 => map(OpCode::RETI, 16, 16, *offset),
        0xDA => map_u16(bytes, 12, 16, offset, |direct| {
            OpCode::JP(Some(OpCond::C), OpVal::D16(direct))
        }),
        0xDC => map_u16(bytes, 12, 24, offset, |direct| {
            OpCode::CALL(Some(OpCond::C), OpVal::D16(direct))
        }),
        0xDE => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::SBC(OpVal::D8(direct))
        }),
        0xDF => map(OpCode::RST(OpVal::D8(0x18)), 16, 16, *offset),
        0xE0 => map_u8(bytes, 12, 12, offset, |direct| {
            OpCode::LDRN(OpVal::A, OpVal::D8(direct))
        }),
        0xE1 => map(OpCode::POP(OpVal::HL), 12, 12, *offset),
        // TODO: Check if this is really two bytes...
        0xE2 => map(OpCode::LDRN(OpVal::A, OpVal::C), 8, 8, *offset),
        0xE5 => map(OpCode::PUSH(OpVal::HL), 16, 16, *offset),
        0xE6 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::AND(OpVal::D8(direct))
        }),
        0xE7 => map(OpCode::RST(OpVal::D8(0x20)), 16, 16, *offset),
        0xE8 => map_u8(bytes, 16, 16, offset, |direct| {
            OpCode::ADD(OpVal::SP, OpVal::D8(direct))
        }),
        0xE9 => map(OpCode::JP(None, OpVal::HL), 4, 4, *offset),
        0xEA => map_u16(bytes, 16, 16, offset, |direct| {
            OpCode::LDL(OpVal::A16(direct), OpVal::A)
        }),
        0xEE => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::XOR(OpVal::D8(direct))
        }),
        0xEF => map(OpCode::RST(OpVal::D8(0x28)), 16, 16, *offset),
        0xF0 => map_u8(bytes, 12, 12, offset, |direct| {
            OpCode::LDLN(OpVal::A, OpVal::D8(direct))
        }),
        0xF1 => map(OpCode::POP(OpVal::AF), 12, 12, *offset),
        // TODO: Check if this is really two bytes...
        0xF2 => map(OpCode::LDLN(OpVal::A, OpVal::C), 8, 8, *offset),
        0xF3 => map(OpCode::DI, 4, 4, *offset),
        0xF5 => map(OpCode::PUSH(OpVal::AF), 16, 16, *offset),
        0xF6 => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::OR(OpVal::D8(direct))
        }),
        0xF7 => map(OpCode::RST(OpVal::D8(0x30)), 16, 16, *offset),
        0xF8 => map_u8(bytes, 12, 12, offset, |direct| {
            OpCode::LDHL(OpVal::D8(direct))
        }),
        0xF9 => map(OpCode::LDL(OpVal::SP, OpVal::HL), 8, 8, *offset),
        0xFA => map_u16(bytes, 16, 16, offset, |direct| {
            OpCode::LDL(OpVal::A, OpVal::A16(direct))
        }),
        0xFB => map(OpCode::EI, 4, 4, *offset),
        0xFE => map_u8(bytes, 8, 8, offset, |direct| {
            OpCode::CP(OpVal::D8(direct))
        }),
        0xFF => map(OpCode::RST(OpVal::D8(0x38)), 16, 16, *offset),
        unknown => map_unknown(unknown, *offset)
    }
}

// Decode a cb code prefixed instruction.
fn decode_cb(bytes: &[u8], offset: &mut usize) -> Result<Instruction, String> {
    let cb_code = get_u8(bytes, offset)?;

    match cb_code {
        0x00 => map_cb(OpCode::RLC(OpVal::B), 8, 8, *offset),
        0x01 => map_cb(OpCode::RLC(OpVal::C), 8, 8, *offset),
        0x02 => map_cb(OpCode::RLC(OpVal::D), 8, 8, *offset),
        0x03 => map_cb(OpCode::RLC(OpVal::E), 8, 8, *offset),
        0x04 => map_cb(OpCode::RLC(OpVal::H), 8, 8, *offset),
        0x05 => map_cb(OpCode::RLC(OpVal::L), 8, 8, *offset),
        0x06 => map_cb(OpCode::RLC(OpVal::AHL), 16, 16, *offset),
        0x07 => map_cb(OpCode::RLC(OpVal::A), 8, 8, *offset),
        0x08 => map_cb(OpCode::RRC(OpVal::B), 8, 8, *offset),
        0x09 => map_cb(OpCode::RRC(OpVal::C), 8, 8, *offset),
        0x0A => map_cb(OpCode::RRC(OpVal::D), 8, 8, *offset),
        0x0B => map_cb(OpCode::RRC(OpVal::E), 8, 8, *offset),
        0x0C => map_cb(OpCode::RRC(OpVal::H), 8, 8, *offset),
        0x0D => map_cb(OpCode::RRC(OpVal::L), 8, 8, *offset),
        0x0E => map_cb(OpCode::RRC(OpVal::AHL), 16, 16, *offset),
        0x0F => map_cb(OpCode::RRC(OpVal::A), 8, 8, *offset),
        0x10 => map_cb(OpCode::RL(OpVal::B), 8, 8, *offset),
        0x11 => map_cb(OpCode::RL(OpVal::C), 8, 8, *offset),
        0x12 => map_cb(OpCode::RL(OpVal::D), 8, 8, *offset),
        0x13 => map_cb(OpCode::RL(OpVal::E), 8, 8, *offset),
        0x14 => map_cb(OpCode::RL(OpVal::H), 8, 8, *offset),
        0x15 => map_cb(OpCode::RL(OpVal::L), 8, 8, *offset),
        0x16 => map_cb(OpCode::RL(OpVal::AHL), 16, 16, *offset),
        0x17 => map_cb(OpCode::RL(OpVal::A), 8, 8, *offset),
        0x18 => map_cb(OpCode::RR(OpVal::B), 8, 8, *offset),
        0x19 => map_cb(OpCode::RR(OpVal::C), 8, 8, *offset),
        0x1A => map_cb(OpCode::RR(OpVal::D), 8, 8, *offset),
        0x1B => map_cb(OpCode::RR(OpVal::E), 8, 8, *offset),
        0x1C => map_cb(OpCode::RR(OpVal::H), 8, 8, *offset),
        0x1D => map_cb(OpCode::RR(OpVal::L), 8, 8, *offset),
        0x1E => map_cb(OpCode::RR(OpVal::AHL), 16, 16, *offset),
        0x1F => map_cb(OpCode::RR(OpVal::A), 8, 8, *offset),
        0x20 => map_cb(OpCode::SLA(OpVal::B), 8, 8, *offset),
        0x21 => map_cb(OpCode::SLA(OpVal::C), 8, 8, *offset),
        0x22 => map_cb(OpCode::SLA(OpVal::D), 8, 8, *offset),
        0x23 => map_cb(OpCode::SLA(OpVal::E), 8, 8, *offset),
        0x24 => map_cb(OpCode::SLA(OpVal::H), 8, 8, *offset),
        0x25 => map_cb(OpCode::SLA(OpVal::L), 8, 8, *offset),
        0x26 => map_cb(OpCode::SLA(OpVal::AHL), 16, 16, *offset),
        0x27 => map_cb(OpCode::SLA(OpVal::A), 8, 8, *offset),
        0x28 => map_cb(OpCode::SRA(OpVal::B), 8, 8, *offset),
        0x29 => map_cb(OpCode::SRA(OpVal::C), 8, 8, *offset),
        0x2A => map_cb(OpCode::SRA(OpVal::D), 8, 8, *offset),
        0x2B => map_cb(OpCode::SRA(OpVal::E), 8, 8, *offset),
        0x2C => map_cb(OpCode::SRA(OpVal::H), 8, 8, *offset),
        0x2D => map_cb(OpCode::SRA(OpVal::L), 8, 8, *offset),
        0x2E => map_cb(OpCode::SRA(OpVal::AHL), 16, 16, *offset),
        0x2F => map_cb(OpCode::SRA(OpVal::A), 8, 8, *offset),
        0x30 => map_cb(OpCode::SWAP(OpVal::B), 8, 8, *offset),
        0x31 => map_cb(OpCode::SWAP(OpVal::C), 8, 8, *offset),
        0x32 => map_cb(OpCode::SWAP(OpVal::D), 8, 8, *offset),
        0x33 => map_cb(OpCode::SWAP(OpVal::E), 8, 8, *offset),
        0x34 => map_cb(OpCode::SWAP(OpVal::H), 8, 8, *offset),
        0x35 => map_cb(OpCode::SWAP(OpVal::L), 8, 8, *offset),
        0x36 => map_cb(OpCode::SWAP(OpVal::AHL), 16, 16, *offset),
        0x37 => map_cb(OpCode::SWAP(OpVal::A), 8, 8, *offset),
        0x38 => map_cb(OpCode::SRL(OpVal::B), 8, 8, *offset),
        0x39 => map_cb(OpCode::SRL(OpVal::C), 8, 8, *offset),
        0x3A => map_cb(OpCode::SRL(OpVal::D), 8, 8, *offset),
        0x3B => map_cb(OpCode::SRL(OpVal::E), 8, 8, *offset),
        0x3C => map_cb(OpCode::SRL(OpVal::H), 8, 8, *offset),
        0x3D => map_cb(OpCode::SRL(OpVal::L), 8, 8, *offset),
        0x3E => map_cb(OpCode::SRL(OpVal::AHL), 16, 16, *offset),
        0x3F => map_cb(OpCode::SRL(OpVal::A), 8, 8, *offset),
        0x40 => map_cb(OpCode::BIT(OpBit::B0, OpVal::B), 8, 8, *offset),
        0x41 => map_cb(OpCode::BIT(OpBit::B0, OpVal::C), 8, 8, *offset),
        0x42 => map_cb(OpCode::BIT(OpBit::B0, OpVal::D), 8, 8, *offset),
        0x43 => map_cb(OpCode::BIT(OpBit::B0, OpVal::E), 8, 8, *offset),
        0x44 => map_cb(OpCode::BIT(OpBit::B0, OpVal::H), 8, 8, *offset),
        0x45 => map_cb(OpCode::BIT(OpBit::B0, OpVal::L), 8, 8, *offset),
        0x46 => map_cb(OpCode::BIT(OpBit::B0, OpVal::AHL), 16, 16, *offset),
        0x47 => map_cb(OpCode::BIT(OpBit::B0, OpVal::A), 8, 8, *offset),
        0x48 => map_cb(OpCode::BIT(OpBit::B1, OpVal::B), 8, 8, *offset),
        0x49 => map_cb(OpCode::BIT(OpBit::B1, OpVal::C), 8, 8, *offset),
        0x4A => map_cb(OpCode::BIT(OpBit::B1, OpVal::D), 8, 8, *offset),
        0x4B => map_cb(OpCode::BIT(OpBit::B1, OpVal::E), 8, 8, *offset),
        0x4C => map_cb(OpCode::BIT(OpBit::B1, OpVal::H), 8, 8, *offset),
        0x4D => map_cb(OpCode::BIT(OpBit::B1, OpVal::L), 8, 8, *offset),
        0x4E => map_cb(OpCode::BIT(OpBit::B1, OpVal::AHL), 16, 16, *offset),
        0x4F => map_cb(OpCode::BIT(OpBit::B1, OpVal::A), 8, 8, *offset),
        0x50 => map_cb(OpCode::BIT(OpBit::B2, OpVal::B), 8, 8, *offset),
        0x51 => map_cb(OpCode::BIT(OpBit::B2, OpVal::C), 8, 8, *offset),
        0x52 => map_cb(OpCode::BIT(OpBit::B2, OpVal::D), 8, 8, *offset),
        0x53 => map_cb(OpCode::BIT(OpBit::B2, OpVal::E), 8, 8, *offset),
        0x54 => map_cb(OpCode::BIT(OpBit::B2, OpVal::H), 8, 8, *offset),
        0x55 => map_cb(OpCode::BIT(OpBit::B2, OpVal::L), 8, 8, *offset),
        0x56 => map_cb(OpCode::BIT(OpBit::B2, OpVal::AHL), 16, 16, *offset),
        0x57 => map_cb(OpCode::BIT(OpBit::B2, OpVal::A), 8, 8, *offset),
        0x58 => map_cb(OpCode::BIT(OpBit::B3, OpVal::B), 8, 8, *offset),
        0x59 => map_cb(OpCode::BIT(OpBit::B3, OpVal::C), 8, 8, *offset),
        0x5A => map_cb(OpCode::BIT(OpBit::B3, OpVal::D), 8, 8, *offset),
        0x5B => map_cb(OpCode::BIT(OpBit::B3, OpVal::E), 8, 8, *offset),
        0x5C => map_cb(OpCode::BIT(OpBit::B3, OpVal::H), 8, 8, *offset),
        0x5D => map_cb(OpCode::BIT(OpBit::B3, OpVal::L), 8, 8, *offset),
        0x5E => map_cb(OpCode::BIT(OpBit::B3, OpVal::AHL), 16, 16, *offset),
        0x5F => map_cb(OpCode::BIT(OpBit::B3, OpVal::A), 8, 8, *offset),
        0x60 => map_cb(OpCode::BIT(OpBit::B4, OpVal::B), 8, 8, *offset),
        0x61 => map_cb(OpCode::BIT(OpBit::B4, OpVal::C), 8, 8, *offset),
        0x62 => map_cb(OpCode::BIT(OpBit::B4, OpVal::D), 8, 8, *offset),
        0x63 => map_cb(OpCode::BIT(OpBit::B4, OpVal::E), 8, 8, *offset),
        0x64 => map_cb(OpCode::BIT(OpBit::B4, OpVal::H), 8, 8, *offset),
        0x65 => map_cb(OpCode::BIT(OpBit::B4, OpVal::L), 8, 8, *offset),
        0x66 => map_cb(OpCode::BIT(OpBit::B4, OpVal::AHL), 16, 16, *offset),
        0x67 => map_cb(OpCode::BIT(OpBit::B4, OpVal::A), 8, 8, *offset),
        0x68 => map_cb(OpCode::BIT(OpBit::B5, OpVal::B), 8, 8, *offset),
        0x69 => map_cb(OpCode::BIT(OpBit::B5, OpVal::C), 8, 8, *offset),
        0x6A => map_cb(OpCode::BIT(OpBit::B5, OpVal::D), 8, 8, *offset),
        0x6B => map_cb(OpCode::BIT(OpBit::B5, OpVal::E), 8, 8, *offset),
        0x6C => map_cb(OpCode::BIT(OpBit::B5, OpVal::H), 8, 8, *offset),
        0x6D => map_cb(OpCode::BIT(OpBit::B5, OpVal::L), 8, 8, *offset),
        0x6E => map_cb(OpCode::BIT(OpBit::B5, OpVal::AHL), 16, 16, *offset),
        0x6F => map_cb(OpCode::BIT(OpBit::B5, OpVal::A), 8, 8, *offset),
        0x70 => map_cb(OpCode::BIT(OpBit::B6, OpVal::B), 8, 8, *offset),
        0x71 => map_cb(OpCode::BIT(OpBit::B6, OpVal::C), 8, 8, *offset),
        0x72 => map_cb(OpCode::BIT(OpBit::B6, OpVal::D), 8, 8, *offset),
        0x73 => map_cb(OpCode::BIT(OpBit::B6, OpVal::E), 8, 8, *offset),
        0x74 => map_cb(OpCode::BIT(OpBit::B6, OpVal::H), 8, 8, *offset),
        0x75 => map_cb(OpCode::BIT(OpBit::B6, OpVal::L), 8, 8, *offset),
        0x76 => map_cb(OpCode::BIT(OpBit::B6, OpVal::AHL), 16, 16, *offset),
        0x77 => map_cb(OpCode::BIT(OpBit::B6, OpVal::A), 8, 8, *offset),
        0x78 => map_cb(OpCode::BIT(OpBit::B7, OpVal::B), 8, 8, *offset),
        0x79 => map_cb(OpCode::BIT(OpBit::B7, OpVal::C), 8, 8, *offset),
        0x7A => map_cb(OpCode::BIT(OpBit::B7, OpVal::D), 8, 8, *offset),
        0x7B => map_cb(OpCode::BIT(OpBit::B7, OpVal::E), 8, 8, *offset),
        0x7C => map_cb(OpCode::BIT(OpBit::B7, OpVal::H), 8, 8, *offset),
        0x7D => map_cb(OpCode::BIT(OpBit::B7, OpVal::L), 8, 8, *offset),
        0x7E => map_cb(OpCode::BIT(OpBit::B7, OpVal::AHL), 16, 16, *offset),
        0x7F => map_cb(OpCode::BIT(OpBit::B7, OpVal::A), 8, 8, *offset),
        0x80 => map_cb(OpCode::RES(OpBit::B0, OpVal::B), 8, 8, *offset),
        0x81 => map_cb(OpCode::RES(OpBit::B0, OpVal::C), 8, 8, *offset),
        0x82 => map_cb(OpCode::RES(OpBit::B0, OpVal::D), 8, 8, *offset),
        0x83 => map_cb(OpCode::RES(OpBit::B0, OpVal::E), 8, 8, *offset),
        0x84 => map_cb(OpCode::RES(OpBit::B0, OpVal::H), 8, 8, *offset),
        0x85 => map_cb(OpCode::RES(OpBit::B0, OpVal::L), 8, 8, *offset),
        0x86 => map_cb(OpCode::RES(OpBit::B0, OpVal::AHL), 16, 16, *offset),
        0x87 => map_cb(OpCode::RES(OpBit::B0, OpVal::A), 8, 8, *offset),
        0x88 => map_cb(OpCode::RES(OpBit::B1, OpVal::B), 8, 8, *offset),
        0x89 => map_cb(OpCode::RES(OpBit::B1, OpVal::C), 8, 8, *offset),
        0x8A => map_cb(OpCode::RES(OpBit::B1, OpVal::D), 8, 8, *offset),
        0x8B => map_cb(OpCode::RES(OpBit::B1, OpVal::E), 8, 8, *offset),
        0x8C => map_cb(OpCode::RES(OpBit::B1, OpVal::H), 8, 8, *offset),
        0x8D => map_cb(OpCode::RES(OpBit::B1, OpVal::L), 8, 8, *offset),
        0x8E => map_cb(OpCode::RES(OpBit::B1, OpVal::AHL), 16, 16, *offset),
        0x8F => map_cb(OpCode::RES(OpBit::B1, OpVal::A), 8, 8, *offset),
        0x90 => map_cb(OpCode::RES(OpBit::B2, OpVal::B), 8, 8, *offset),
        0x91 => map_cb(OpCode::RES(OpBit::B2, OpVal::C), 8, 8, *offset),
        0x92 => map_cb(OpCode::RES(OpBit::B2, OpVal::D), 8, 8, *offset),
        0x93 => map_cb(OpCode::RES(OpBit::B2, OpVal::E), 8, 8, *offset),
        0x94 => map_cb(OpCode::RES(OpBit::B2, OpVal::H), 8, 8, *offset),
        0x95 => map_cb(OpCode::RES(OpBit::B2, OpVal::L), 8, 8, *offset),
        0x96 => map_cb(OpCode::RES(OpBit::B2, OpVal::AHL), 16, 16, *offset),
        0x97 => map_cb(OpCode::RES(OpBit::B2, OpVal::A), 8, 8, *offset),
        0x98 => map_cb(OpCode::RES(OpBit::B3, OpVal::B), 8, 8, *offset),
        0x99 => map_cb(OpCode::RES(OpBit::B3, OpVal::C), 8, 8, *offset),
        0x9A => map_cb(OpCode::RES(OpBit::B3, OpVal::D), 8, 8, *offset),
        0x9B => map_cb(OpCode::RES(OpBit::B3, OpVal::E), 8, 8, *offset),
        0x9C => map_cb(OpCode::RES(OpBit::B3, OpVal::H), 8, 8, *offset),
        0x9D => map_cb(OpCode::RES(OpBit::B3, OpVal::L), 8, 8, *offset),
        0x9E => map_cb(OpCode::RES(OpBit::B3, OpVal::AHL), 16, 16, *offset),
        0x9F => map_cb(OpCode::RES(OpBit::B3, OpVal::A), 8, 8, *offset),
        0xA0 => map_cb(OpCode::RES(OpBit::B4, OpVal::B), 8, 8, *offset),
        0xA1 => map_cb(OpCode::RES(OpBit::B4, OpVal::C), 8, 8, *offset),
        0xA2 => map_cb(OpCode::RES(OpBit::B4, OpVal::D), 8, 8, *offset),
        0xA3 => map_cb(OpCode::RES(OpBit::B4, OpVal::E), 8, 8, *offset),
        0xA4 => map_cb(OpCode::RES(OpBit::B4, OpVal::H), 8, 8, *offset),
        0xA5 => map_cb(OpCode::RES(OpBit::B4, OpVal::L), 8, 8, *offset),
        0xA6 => map_cb(OpCode::RES(OpBit::B4, OpVal::AHL), 16, 16, *offset),
        0xA7 => map_cb(OpCode::RES(OpBit::B4, OpVal::A), 8, 8, *offset),
        0xA8 => map_cb(OpCode::RES(OpBit::B5, OpVal::B), 8, 8, *offset),
        0xA9 => map_cb(OpCode::RES(OpBit::B5, OpVal::C), 8, 8, *offset),
        0xAA => map_cb(OpCode::RES(OpBit::B5, OpVal::D), 8, 8, *offset),
        0xAB => map_cb(OpCode::RES(OpBit::B5, OpVal::E), 8, 8, *offset),
        0xAC => map_cb(OpCode::RES(OpBit::B5, OpVal::H), 8, 8, *offset),
        0xAD => map_cb(OpCode::RES(OpBit::B5, OpVal::L), 8, 8, *offset),
        0xAE => map_cb(OpCode::RES(OpBit::B5, OpVal::AHL), 16, 16, *offset),
        0xAF => map_cb(OpCode::RES(OpBit::B5, OpVal::A), 8, 8, *offset),
        0xB0 => map_cb(OpCode::RES(OpBit::B6, OpVal::B), 8, 8, *offset),
        0xB1 => map_cb(OpCode::RES(OpBit::B6, OpVal::C), 8, 8, *offset),
        0xB2 => map_cb(OpCode::RES(OpBit::B6, OpVal::D), 8, 8, *offset),
        0xB3 => map_cb(OpCode::RES(OpBit::B6, OpVal::E), 8, 8, *offset),
        0xB4 => map_cb(OpCode::RES(OpBit::B6, OpVal::H), 8, 8, *offset),
        0xB5 => map_cb(OpCode::RES(OpBit::B6, OpVal::L), 8, 8, *offset),
        0xB6 => map_cb(OpCode::RES(OpBit::B6, OpVal::AHL), 16, 16, *offset),
        0xB7 => map_cb(OpCode::RES(OpBit::B6, OpVal::A), 8, 8, *offset),
        0xB8 => map_cb(OpCode::RES(OpBit::B7, OpVal::B), 8, 8, *offset),
        0xB9 => map_cb(OpCode::RES(OpBit::B7, OpVal::C), 8, 8, *offset),
        0xBA => map_cb(OpCode::RES(OpBit::B7, OpVal::D), 8, 8, *offset),
        0xBB => map_cb(OpCode::RES(OpBit::B7, OpVal::E), 8, 8, *offset),
        0xBC => map_cb(OpCode::RES(OpBit::B7, OpVal::H), 8, 8, *offset),
        0xBD => map_cb(OpCode::RES(OpBit::B7, OpVal::L), 8, 8, *offset),
        0xBE => map_cb(OpCode::RES(OpBit::B7, OpVal::AHL), 16, 16, *offset),
        0xBF => map_cb(OpCode::RES(OpBit::B7, OpVal::A), 8, 8, *offset),
        0xC0 => map_cb(OpCode::SET(OpBit::B0, OpVal::B), 8, 8, *offset),
        0xC1 => map_cb(OpCode::SET(OpBit::B0, OpVal::C), 8, 8, *offset),
        0xC2 => map_cb(OpCode::SET(OpBit::B0, OpVal::D), 8, 8, *offset),
        0xC3 => map_cb(OpCode::SET(OpBit::B0, OpVal::E), 8, 8, *offset),
        0xC4 => map_cb(OpCode::SET(OpBit::B0, OpVal::H), 8, 8, *offset),
        0xC5 => map_cb(OpCode::SET(OpBit::B0, OpVal::L), 8, 8, *offset),
        0xC6 => map_cb(OpCode::SET(OpBit::B0, OpVal::AHL), 16, 16, *offset),
        0xC7 => map_cb(OpCode::SET(OpBit::B0, OpVal::A), 8, 8, *offset),
        0xC8 => map_cb(OpCode::SET(OpBit::B1, OpVal::B), 8, 8, *offset),
        0xC9 => map_cb(OpCode::SET(OpBit::B1, OpVal::C), 8, 8, *offset),
        0xCA => map_cb(OpCode::SET(OpBit::B1, OpVal::D), 8, 8, *offset),
        0xCB => map_cb(OpCode::SET(OpBit::B1, OpVal::E), 8, 8, *offset),
        0xCC => map_cb(OpCode::SET(OpBit::B1, OpVal::H), 8, 8, *offset),
        0xCD => map_cb(OpCode::SET(OpBit::B1, OpVal::L), 8, 8, *offset),
        0xCE => map_cb(OpCode::SET(OpBit::B1, OpVal::AHL), 16, 16, *offset),
        0xCF => map_cb(OpCode::SET(OpBit::B1, OpVal::A), 8, 8, *offset),
        0xD0 => map_cb(OpCode::SET(OpBit::B2, OpVal::B), 8, 8, *offset),
        0xD1 => map_cb(OpCode::SET(OpBit::B2, OpVal::C), 8, 8, *offset),
        0xD2 => map_cb(OpCode::SET(OpBit::B2, OpVal::D), 8, 8, *offset),
        0xD3 => map_cb(OpCode::SET(OpBit::B2, OpVal::E), 8, 8, *offset),
        0xD4 => map_cb(OpCode::SET(OpBit::B2, OpVal::H), 8, 8, *offset),
        0xD5 => map_cb(OpCode::SET(OpBit::B2, OpVal::L), 8, 8, *offset),
        0xD6 => map_cb(OpCode::SET(OpBit::B2, OpVal::AHL), 16, 16, *offset),
        0xD7 => map_cb(OpCode::SET(OpBit::B2, OpVal::A), 8, 8, *offset),
        0xD8 => map_cb(OpCode::SET(OpBit::B3, OpVal::B), 8, 8, *offset),
        0xD9 => map_cb(OpCode::SET(OpBit::B3, OpVal::C), 8, 8, *offset),
        0xDA => map_cb(OpCode::SET(OpBit::B3, OpVal::D), 8, 8, *offset),
        0xDB => map_cb(OpCode::SET(OpBit::B3, OpVal::E), 8, 8, *offset),
        0xDC => map_cb(OpCode::SET(OpBit::B3, OpVal::H), 8, 8, *offset),
        0xDD => map_cb(OpCode::SET(OpBit::B3, OpVal::L), 8, 8, *offset),
        0xDE => map_cb(OpCode::SET(OpBit::B3, OpVal::AHL), 16, 16, *offset),
        0xDF => map_cb(OpCode::SET(OpBit::B3, OpVal::A), 8, 8, *offset),
        0xE0 => map_cb(OpCode::SET(OpBit::B4, OpVal::B), 8, 8, *offset),
        0xE1 => map_cb(OpCode::SET(OpBit::B4, OpVal::C), 8, 8, *offset),
        0xE2 => map_cb(OpCode::SET(OpBit::B4, OpVal::D), 8, 8, *offset),
        0xE3 => map_cb(OpCode::SET(OpBit::B4, OpVal::E), 8, 8, *offset),
        0xE4 => map_cb(OpCode::SET(OpBit::B4, OpVal::H), 8, 8, *offset),
        0xE5 => map_cb(OpCode::SET(OpBit::B4, OpVal::L), 8, 8, *offset),
        0xE6 => map_cb(OpCode::SET(OpBit::B4, OpVal::AHL), 16, 16, *offset),
        0xE7 => map_cb(OpCode::SET(OpBit::B4, OpVal::A), 8, 8, *offset),
        0xE8 => map_cb(OpCode::SET(OpBit::B5, OpVal::B), 8, 8, *offset),
        0xE9 => map_cb(OpCode::SET(OpBit::B5, OpVal::C), 8, 8, *offset),
        0xEA => map_cb(OpCode::SET(OpBit::B5, OpVal::D), 8, 8, *offset),
        0xEB => map_cb(OpCode::SET(OpBit::B5, OpVal::E), 8, 8, *offset),
        0xEC => map_cb(OpCode::SET(OpBit::B5, OpVal::H), 8, 8, *offset),
        0xED => map_cb(OpCode::SET(OpBit::B5, OpVal::L), 8, 8, *offset),
        0xEE => map_cb(OpCode::SET(OpBit::B5, OpVal::AHL), 16, 16, *offset),
        0xEF => map_cb(OpCode::SET(OpBit::B5, OpVal::A), 8, 8, *offset),
        0xF0 => map_cb(OpCode::SET(OpBit::B6, OpVal::B), 8, 8, *offset),
        0xF1 => map_cb(OpCode::SET(OpBit::B6, OpVal::C), 8, 8, *offset),
        0xF2 => map_cb(OpCode::SET(OpBit::B6, OpVal::D), 8, 8, *offset),
        0xF3 => map_cb(OpCode::SET(OpBit::B6, OpVal::E), 8, 8, *offset),
        0xF4 => map_cb(OpCode::SET(OpBit::B6, OpVal::H), 8, 8, *offset),
        0xF5 => map_cb(OpCode::SET(OpBit::B6, OpVal::L), 8, 8, *offset),
        0xF6 => map_cb(OpCode::SET(OpBit::B6, OpVal::AHL), 16, 16, *offset),
        0xF7 => map_cb(OpCode::SET(OpBit::B6, OpVal::A), 8, 8, *offset),
        0xF8 => map_cb(OpCode::SET(OpBit::B7, OpVal::B), 8, 8, *offset),
        0xF9 => map_cb(OpCode::SET(OpBit::B7, OpVal::C), 8, 8, *offset),
        0xFA => map_cb(OpCode::SET(OpBit::B7, OpVal::D), 8, 8, *offset),
        0xFB => map_cb(OpCode::SET(OpBit::B7, OpVal::E), 8, 8, *offset),
        0xFC => map_cb(OpCode::SET(OpBit::B7, OpVal::H), 8, 8, *offset),
        0xFD => map_cb(OpCode::SET(OpBit::B7, OpVal::L), 8, 8, *offset),
        0xFE => map_cb(OpCode::SET(OpBit::B7, OpVal::AHL), 16, 16, *offset),
        0xFF => map_cb(OpCode::SET(OpBit::B7, OpVal::A), 8, 8, *offset),
        //unknown => map_cb_unknown(unknown, *offset)
    }
}

// Get a single u8 at the given offset and increment the offset.
fn get_u8(bytes: &[u8], offset: &mut usize) -> Result<u8, String> {
    if bytes.len() > *offset {
        let byte = bytes[*offset];
        *offset += 1;

        Ok(byte)
    } else {
        return Err(format!("expected 1 byte at offset {}", *offset))
    }
}

// Get a single u16 at the given offset and increment the offset.
fn get_u16(bytes: &[u8], offset: &mut usize) -> Result<u16, String> {
    if bytes.len() > *offset + 1 {
        let byte_one = get_u8(bytes, offset)?;
        let byte_two = get_u8(bytes, offset)?;

        Ok(((byte_one as u16) << 8) | (byte_two as u16))
    } else {
        return Err(format!("expected 2 bytes at offset {}", *offset))
    }
}

// Map an operation into an instruction.
fn map(code: OpCode, normal_cycles: u8, branch_cycles: u8, offset: usize) -> Result<Instruction, String> {
    Ok(Instruction{ code, normal_cycles, branch_cycles, instr_size: 1, offset })
}

// Map a cb prefixed operation into an instruction.
fn map_cb(code: OpCode, normal_cycles: u8, branch_cycles: u8, offset: usize) -> Result<Instruction, String> {
    let mut instr = map(code, normal_cycles, branch_cycles, offset)?;
    instr.instr_size = 2;

    Ok(instr)
}

// Map an operation that takes one additional byte into an instruction.
fn map_u8(bytes: &[u8], normal_cycles: u8, branch_cycles: u8, offset: &mut usize, mapper: fn(u8) -> OpCode) -> Result<Instruction, String> {
    let value = get_u8(bytes, offset)?;
    let op = mapper(value);

    let mut instr = map(op, normal_cycles, branch_cycles, *offset)?;
    instr.instr_size = 2;

    Ok(instr)
}

// Map an operation that takes two additional bytes into an instruction.
fn map_u16(bytes: &[u8], normal_cycles: u8, branch_cycles: u8, offset: &mut usize, mapper: fn(u16) -> OpCode) -> Result<Instruction, String> {
    let value = get_u16(bytes, offset)?;
    let op = mapper(value);

    let mut instr = map(op, normal_cycles, branch_cycles, *offset)?;
    instr.instr_size = 3;

    Ok(instr)
}

// Maps to an unknown error for a code.
fn map_unknown(code: u8, offset: usize) -> Result<Instruction, String> {
    Err(format!("unknown instruction 0x{:X?} at offset {}", code, offset))
}

// Maps to an unknown error for a cb prefixed code.
fn map_cb_unknown(code: u8, offset: usize) -> Result<Instruction, String> {
    Err(format!("unknown cb instruction 0x{:X?} at offset {}", code, offset))
}