#[derive(Debug)]
pub enum OpVal {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    D8(u8),
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

#[derive(Debug)]
pub enum OpCond {
    NZ,
    Z,
    NC,
    C
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Instruction {
    code: OpCode,
    offset: usize
}

impl Instruction {
    pub fn from_bytes(mut bytes: &[u8]) -> Result<Vec<Instruction>, String> {
        let mut instructions = Vec::new();
        let mut curr_offset = 0;

        while !bytes.is_empty() {
            let byte_code = bytes[0];
            let offset = curr_offset;

            bytes = &bytes[1..];
            curr_offset += 1;

            let (code, size) = decode(byte_code, bytes, curr_offset)?;
            bytes = &bytes[size..];
            curr_offset += size;

            instructions.push(Instruction{ code, offset });
        }

        return Ok(instructions)
    }
}

// TODO: Add cycle count to return values, some instructions have different cycles depending on branch
// Decode the instruction identified by the code using the given instructions buffer.
//
// http://www.pastraiser.com/cpu/gameboy/gameboy_opcodes.html
fn decode(code: u8, bytes: &[u8], offset: usize) -> Result<(OpCode, usize), String> {
    match code {
        0x00 => map(OpCode::NOP),
        0x01 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::BC, OpVal::D16(direct))
        }),
        0x02 => map(OpCode::LDL(OpVal::ABC, OpVal::A)),
        0x03 => map(OpCode::INC(OpVal::BC)),
        0x04 => map(OpCode::INC(OpVal::B)),
        0x05 => map(OpCode::DEC(OpVal::B)),
        0x06 => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::B, OpVal::D8(direct))
        }),
        0x07 => map(OpCode::RLC(OpVal::A)),
        0x08 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::SP, OpVal::D16(direct))
        }),
        0x09 => map(OpCode::ADD(OpVal::HL, OpVal::BC)),
        0x0A => map(OpCode::LDL(OpVal::A, OpVal::ABC)),
        0x0B => map(OpCode::DEC(OpVal::BC)),
        0x0C => map(OpCode::INC(OpVal::C)),
        0x0D => map(OpCode::DEC(OpVal::C)),
        0x0E => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::C, OpVal::D8(direct))
        }),
        0x0F => map(OpCode::RRC(OpVal::A)),
        // TODO: Make sure this is correct, assuming 1 dead byte (0x00) after code
        0x10 => Ok((OpCode::STOP, 1)),
        0x11 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::DE, OpVal::D16(direct))
        }),
        0x12 => map(OpCode::LDL(OpVal::ADE, OpVal::A)),
        0x13 => map(OpCode::INC(OpVal::DE)),
        0x14 => map(OpCode::INC(OpVal::D)),
        0x15 => map(OpCode::DEC(OpVal::D)),
        0x16 => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::D, OpVal::D8(direct))
        }),
        0x17 => map(OpCode::RL(OpVal::A)),
        0x18 => map_u8(bytes, offset, |direct| {
            OpCode::JR(Option::None, OpVal::D8(direct))
        }),
        0x19 => map(OpCode::ADD(OpVal::HL, OpVal::DE)),
        0x1A => map(OpCode::LDL(OpVal::A, OpVal::ADE)),
        0x1B => map(OpCode::DEC(OpVal::DE)),
        0x1C => map(OpCode::INC(OpVal::E)),
        0x1D => map(OpCode::DEC(OpVal::E)),
        0x1E => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::E, OpVal::D8(direct))
        }),
        0x1F => map(OpCode::RR(OpVal::A)),
        0x20 => map_u8(bytes, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::NZ), OpVal::D8(direct))
        }),
        0x21 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::DE, OpVal::D16(direct))
        }),
        0x22 => map(OpCode::LDRI(OpVal::A, OpVal::AHL)),
        0x23 => map(OpCode::INC(OpVal::HL)),
        0x24 => map(OpCode::INC(OpVal::H)),
        0x25 => map(OpCode::DEC(OpVal::H)),
        0x26 => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::H, OpVal::D8(direct))
        }),
        0x27 => map(OpCode::DAA),
        0x28 => map_u8(bytes, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::Z), OpVal::D8(direct))
        }),
        0x29 => map(OpCode::ADD(OpVal::HL, OpVal::HL)),
        0x2A => map(OpCode::LDLI(OpVal::A, OpVal::AHL)),
        0x2B => map(OpCode::DEC(OpVal::HL)),
        0x2C => map(OpCode::INC(OpVal::L)),
        0x2D => map(OpCode::DEC(OpVal::L)),
        0x2E => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::L, OpVal::D8(direct))
        }),
        0x2F => map(OpCode::CPL),
        0x30 => map_u8(bytes, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::NC), OpVal::D8(direct))
        }),
        0x31 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::SP, OpVal::D16(direct))
        }),
        0x32 => map(OpCode::LDRD(OpVal::A, OpVal::AHL)),
        0x33 => map(OpCode::INC(OpVal::SP)),
        0x34 => map(OpCode::INC(OpVal::AHL)),
        0x35 => map(OpCode::DEC(OpVal::AHL)),
        0x36 => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::AHL, OpVal::D8(direct))
        }),
        0x37 => map(OpCode::SCF),
        0x38 => map_u8(bytes, offset, |direct| {
            OpCode::JR(Option::Some(OpCond::C), OpVal::D8(direct))
        }),
        0x39 => map(OpCode::ADD(OpVal::HL, OpVal::SP)),
        0x3A => map(OpCode::LDLD(OpVal::A, OpVal::HL)),
        0x3B => map(OpCode::DEC(OpVal::SP)),
        0x3C => map(OpCode::INC(OpVal::A)),
        0x3D => map(OpCode::DEC(OpVal::A)),
        0x3E => map_u8(bytes, offset, |direct| {
            OpCode::LDL(OpVal::A, OpVal::D8(direct))
        }),
        0x3F => map(OpCode::CCF),
        0x40 => map(OpCode::LDL(OpVal::B, OpVal::B)),
        0x41 => map(OpCode::LDL(OpVal::B, OpVal::C)),
        0x42 => map(OpCode::LDL(OpVal::B, OpVal::D)),
        0x43 => map(OpCode::LDL(OpVal::B, OpVal::E)),
        0x44 => map(OpCode::LDL(OpVal::B, OpVal::H)),
        0x45 => map(OpCode::LDL(OpVal::B, OpVal::L)),
        0x46 => map(OpCode::LDL(OpVal::B, OpVal::AHL)),
        0x47 => map(OpCode::LDL(OpVal::B, OpVal::A)),
        0x48 => map(OpCode::LDL(OpVal::C, OpVal::B)),
        0x49 => map(OpCode::LDL(OpVal::C, OpVal::C)),
        0x4A => map(OpCode::LDL(OpVal::C, OpVal::D)),
        0x4B => map(OpCode::LDL(OpVal::C, OpVal::E)),
        0x4C => map(OpCode::LDL(OpVal::C, OpVal::H)),
        0x4D => map(OpCode::LDL(OpVal::C, OpVal::L)),
        0x4E => map(OpCode::LDL(OpVal::C, OpVal::AHL)),
        0x4F => map(OpCode::LDL(OpVal::C, OpVal::A)),
        0x50 => map(OpCode::LDL(OpVal::D, OpVal::B)),
        0x51 => map(OpCode::LDL(OpVal::D, OpVal::C)),
        0x52 => map(OpCode::LDL(OpVal::D, OpVal::D)),
        0x53 => map(OpCode::LDL(OpVal::D, OpVal::E)),
        0x54 => map(OpCode::LDL(OpVal::D, OpVal::H)),
        0x55 => map(OpCode::LDL(OpVal::D, OpVal::L)),
        0x56 => map(OpCode::LDL(OpVal::D, OpVal::AHL)),
        0x57 => map(OpCode::LDL(OpVal::D, OpVal::A)),
        0x58 => map(OpCode::LDL(OpVal::E, OpVal::B)),
        0x59 => map(OpCode::LDL(OpVal::E, OpVal::C)),
        0x5A => map(OpCode::LDL(OpVal::E, OpVal::D)),
        0x5B => map(OpCode::LDL(OpVal::E, OpVal::E)),
        0x5C => map(OpCode::LDL(OpVal::E, OpVal::H)),
        0x5D => map(OpCode::LDL(OpVal::E, OpVal::L)),
        0x5E => map(OpCode::LDL(OpVal::E, OpVal::AHL)),
        0x5F => map(OpCode::LDL(OpVal::E, OpVal::A)),
        0x60 => map(OpCode::LDL(OpVal::H, OpVal::B)),
        0x61 => map(OpCode::LDL(OpVal::H, OpVal::C)),
        0x62 => map(OpCode::LDL(OpVal::H, OpVal::D)),
        0x63 => map(OpCode::LDL(OpVal::H, OpVal::E)),
        0x64 => map(OpCode::LDL(OpVal::H, OpVal::H)),
        0x65 => map(OpCode::LDL(OpVal::H, OpVal::L)),
        0x66 => map(OpCode::LDL(OpVal::H, OpVal::AHL)),
        0x67 => map(OpCode::LDL(OpVal::H, OpVal::A)),
        0x68 => map(OpCode::LDL(OpVal::L, OpVal::B)),
        0x69 => map(OpCode::LDL(OpVal::L, OpVal::C)),
        0x6A => map(OpCode::LDL(OpVal::L, OpVal::D)),
        0x6B => map(OpCode::LDL(OpVal::L, OpVal::E)),
        0x6C => map(OpCode::LDL(OpVal::L, OpVal::H)),
        0x6D => map(OpCode::LDL(OpVal::L, OpVal::L)),
        0x6E => map(OpCode::LDL(OpVal::L, OpVal::AHL)),
        0x6F => map(OpCode::LDL(OpVal::L, OpVal::A)),
        0x70 => map(OpCode::LDL(OpVal::AHL, OpVal::B)),
        0x71 => map(OpCode::LDL(OpVal::AHL, OpVal::C)),
        0x72 => map(OpCode::LDL(OpVal::AHL, OpVal::D)),
        0x73 => map(OpCode::LDL(OpVal::AHL, OpVal::E)),
        0x74 => map(OpCode::LDL(OpVal::AHL, OpVal::H)),
        0x75 => map(OpCode::LDL(OpVal::AHL, OpVal::L)),
        0x76 => map(OpCode::HALT),
        0x77 => map(OpCode::LDL(OpVal::AHL, OpVal::A)),
        0x78 => map(OpCode::LDL(OpVal::A, OpVal::B)),
        0x79 => map(OpCode::LDL(OpVal::A, OpVal::C)),
        0x7A => map(OpCode::LDL(OpVal::A, OpVal::D)),
        0x7B => map(OpCode::LDL(OpVal::A, OpVal::E)),
        0x7C => map(OpCode::LDL(OpVal::A, OpVal::H)),
        0x7D => map(OpCode::LDL(OpVal::A, OpVal::L)),
        0x7E => map(OpCode::LDL(OpVal::A, OpVal::AHL)),
        0x7F => map(OpCode::LDL(OpVal::A, OpVal::A)),
        0x80 => map(OpCode::ADD(OpVal::A, OpVal::B)),
        0x81 => map(OpCode::ADD(OpVal::A, OpVal::C)),
        0x82 => map(OpCode::ADD(OpVal::A, OpVal::D)),
        0x83 => map(OpCode::ADD(OpVal::A, OpVal::E)),
        0x84 => map(OpCode::ADD(OpVal::A, OpVal::H)),
        0x85 => map(OpCode::ADD(OpVal::A, OpVal::L)),
        0x86 => map(OpCode::ADD(OpVal::A, OpVal::AHL)),
        0x87 => map(OpCode::ADD(OpVal::A, OpVal::A)),
        0x88 => map(OpCode::ADC(OpVal::B)),
        0x89 => map(OpCode::ADC(OpVal::C)),
        0x8A => map(OpCode::ADC(OpVal::D)),
        0x8B => map(OpCode::ADC(OpVal::E)),
        0x8C => map(OpCode::ADC(OpVal::H)),
        0x8D => map(OpCode::ADC(OpVal::L)),
        0x8E => map(OpCode::ADC(OpVal::AHL)),
        0x8F => map(OpCode::ADC(OpVal::A)),
        0x90 => map(OpCode::SUB(OpVal::B)),
        0x91 => map(OpCode::SUB(OpVal::C)),
        0x92 => map(OpCode::SUB(OpVal::D)),
        0x93 => map(OpCode::SUB(OpVal::E)),
        0x94 => map(OpCode::SUB(OpVal::H)),
        0x95 => map(OpCode::SUB(OpVal::L)),
        0x96 => map(OpCode::SUB(OpVal::AHL)),
        0x97 => map(OpCode::SUB(OpVal::A)),
        0x98 => map(OpCode::SBC(OpVal::B)),
        0x99 => map(OpCode::SBC(OpVal::C)),
        0x9A => map(OpCode::SBC(OpVal::D)),
        0x9B => map(OpCode::SBC(OpVal::E)),
        0x9C => map(OpCode::SBC(OpVal::H)),
        0x9D => map(OpCode::SBC(OpVal::L)),
        0x9E => map(OpCode::SBC(OpVal::AHL)),
        0x9F => map(OpCode::SBC(OpVal::A)),
        0xA0 => map(OpCode::AND(OpVal::B)),
        0xA1 => map(OpCode::AND(OpVal::C)),
        0xA2 => map(OpCode::AND(OpVal::D)),
        0xA3 => map(OpCode::AND(OpVal::E)),
        0xA4 => map(OpCode::AND(OpVal::H)),
        0xA5 => map(OpCode::AND(OpVal::L)),
        0xA6 => map(OpCode::AND(OpVal::AHL)),
        0xA7 => map(OpCode::AND(OpVal::A)),
        0xA8 => map(OpCode::XOR(OpVal::B)),
        0xA9 => map(OpCode::XOR(OpVal::C)),
        0xAA => map(OpCode::XOR(OpVal::D)),
        0xAB => map(OpCode::XOR(OpVal::E)),
        0xAC => map(OpCode::XOR(OpVal::H)),
        0xAD => map(OpCode::XOR(OpVal::L)),
        0xAE => map(OpCode::XOR(OpVal::AHL)),
        0xAF => map(OpCode::XOR(OpVal::A)),
        0xB0 => map(OpCode::OR(OpVal::B)),
        0xB1 => map(OpCode::OR(OpVal::C)),
        0xB2 => map(OpCode::OR(OpVal::D)),
        0xB3 => map(OpCode::OR(OpVal::E)),
        0xB4 => map(OpCode::OR(OpVal::H)),
        0xB5 => map(OpCode::OR(OpVal::L)),
        0xB6 => map(OpCode::OR(OpVal::AHL)),
        0xB7 => map(OpCode::OR(OpVal::A)),
        0xB8 => map(OpCode::CP(OpVal::B)),
        0xB9 => map(OpCode::CP(OpVal::C)),
        0xBA => map(OpCode::CP(OpVal::D)),
        0xBB => map(OpCode::CP(OpVal::E)),
        0xBC => map(OpCode::CP(OpVal::H)),
        0xBD => map(OpCode::CP(OpVal::L)),
        0xBE => map(OpCode::CP(OpVal::AHL)),
        0xBF => map(OpCode::CP(OpVal::A)),
        0xC0 => map(OpCode::RET(Some(OpCond::NZ))),
        0xC1 => map(OpCode::POP(OpVal::BC)),
        0xC2 => map_u16(bytes, offset, |direct| {
            OpCode::JP(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xC3 => map_u16(bytes, offset, |direct| {
            OpCode::JP(None, OpVal::D16(direct))
        }),
        0xC4 => map_u16(bytes, offset, |direct| {
            OpCode::CALL(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xC5 => map(OpCode::PUSH(OpVal::BC)),
        0xC6 => map_u8(bytes, offset, |direct| {
            OpCode::ADD(OpVal::A, OpVal::D8(direct))
        }),
        0xC7 => map(OpCode::RST(OpVal::D8(0x00))),
        0xC8 => map(OpCode::RET(Some(OpCond::Z))),
        0xC9 => map(OpCode::RET(None)),
        0xCA => map_u16(bytes, offset, |direct| {
            OpCode::JP(Some(OpCond::Z), OpVal::D16(direct))
        }),
        0xCB => decode_cb(bytes, offset),
        0xCC => map_u16(bytes, offset, |direct| {
            OpCode::CALL(Some(OpCond::Z), OpVal::D16(direct))
        }),
        0xCD => map_u16(bytes, offset, |direct| {
            OpCode::CALL(None, OpVal::D16(direct))
        }),
        0xCE => map_u8(bytes, offset, |direct| {
            OpCode::ADC(OpVal::D8(direct))
        }),
        0xCF => map(OpCode::RST(OpVal::D8(0x08))),
        0xD0 => map(OpCode::RET(Some(OpCond::NC))),
        0xD1 => map(OpCode::POP(OpVal::DE)),
        0xD2 => map_u16(bytes, offset, |direct| {
            OpCode::JP(Some(OpCond::NC), OpVal::D16(direct))
        }),
        0xD4 => map_u16(bytes, offset, |direct| {
            OpCode::CALL(Some(OpCond::NZ), OpVal::D16(direct))
        }),
        0xD5 => map(OpCode::PUSH(OpVal::DE)),
        0xD6 => map_u8(bytes, offset, |direct| {
            OpCode::SUB(OpVal::D8(direct))
        }),
        0xD7 => map(OpCode::RST(OpVal::D8(0x10))),
        0xD8 => map(OpCode::RET(Some(OpCond::C))),
        0xD9 => map(OpCode::RETI),
        0xDA => map_u16(bytes, offset, |direct| {
            OpCode::JP(Some(OpCond::C), OpVal::D16(direct))
        }),
        0xDC => map_u16(bytes, offset, |direct| {
            OpCode::CALL(Some(OpCond::C), OpVal::D16(direct))
        }),
        0xDE => map_u8(bytes, offset, |direct| {
            OpCode::SBC(OpVal::D8(direct))
        }),
        0xDF => map(OpCode::RST(OpVal::D8(0x18))),
        0xE0 => map_u8(bytes, offset, |direct| {
            OpCode::LDRN(OpVal::A, OpVal::D8(direct))
        }),
        0xE1 => map(OpCode::POP(OpVal::HL)),
        0xE2 => map_u8(bytes, offset, |direct| {
            OpCode::LDRN(OpVal::A, OpVal::C)
        }),
        0xE5 => map(OpCode::PUSH(OpVal::HL)),
        0xE6 => map_u8(bytes, offset, |direct| {
            OpCode::AND(OpVal::D8(direct))
        }),
        0xE7 => map(OpCode::RST(OpVal::D8(0x20))),
        0xE8 => map_u8(bytes, offset, |direct| {
            OpCode::ADD(OpVal::SP, OpVal::D8(direct))
        }),
        0xE9 => map(OpCode::JP(None, OpVal::HL)),
        0xEA => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::A16(direct), OpVal::A)
        }),
        0xEE => map_u8(bytes, offset, |direct| {
            OpCode::XOR(OpVal::D8(direct))
        }),
        0xEF => map(OpCode::RST(OpVal::D8(0x28))),
        0xF0 => map_u8(bytes, offset, |direct| {
            OpCode::LDLN(OpVal::A, OpVal::D8(direct))
        }),
        0xF1 => map(OpCode::POP(OpVal::AF)),
        0xF2 => map_u8(bytes, offset, |direct| {
            OpCode::LDLN(OpVal::A, OpVal::C)
        }),
        0xF3 => map(OpCode::DI),
        0xF5 => map(OpCode::PUSH(OpVal::AF)),
        0xF6 => map_u8(bytes, offset, |direct| {
            OpCode::OR(OpVal::D8(direct))
        }),
        0xF7 => map(OpCode::RST(OpVal::D8(0x30))),
        0xF8 => map_u8(bytes, offset, |direct| {
            OpCode::LDHL(OpVal::D8(direct))
        }),
        0xF9 => map(OpCode::LDL(OpVal::SP, OpVal::HL)),
        0xFA => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::A, OpVal::A16(direct))
        }),
        0xFB => map(OpCode::EI),
        0xFE => map_u8(bytes, offset, |direct| {
            OpCode::CP(OpVal::D8(direct))
        }),
        0xFF => map(OpCode::RST(OpVal::D8(0x38))),
        unknown => map_unknown(unknown, offset)
    }
}

// Decode a cb code prefixed instruction.
fn decode_cb(bytes: &[u8], offset: usize) -> Result<(OpCode, usize), String> {
    let (cb_code, _) = map_u8(bytes, offset, |cb_code| cb_code)?;

    match cb_code {
        0x00 => map_cb(OpCode::RLC(OpVal::B)),
        0x01 => map_cb(OpCode::RLC(OpVal::C)),
        0x02 => map_cb(OpCode::RLC(OpVal::D)),
        0x03 => map_cb(OpCode::RLC(OpVal::E)),
        0x04 => map_cb(OpCode::RLC(OpVal::H)),
        0x05 => map_cb(OpCode::RLC(OpVal::L)),
        0x06 => map_cb(OpCode::RLC(OpVal::AHL)),
        0x07 => map_cb(OpCode::RLC(OpVal::A)),
        0x08 => map_cb(OpCode::RRC(OpVal::B)),
        0x09 => map_cb(OpCode::RRC(OpVal::C)),
        0x0A => map_cb(OpCode::RRC(OpVal::D)),
        0x0B => map_cb(OpCode::RRC(OpVal::E)),
        0x0C => map_cb(OpCode::RRC(OpVal::H)),
        0x0D => map_cb(OpCode::RRC(OpVal::L)),
        0x0E => map_cb(OpCode::RRC(OpVal::AHL)),
        0x0F => map_cb(OpCode::RRC(OpVal::A)),
        0x10 => map_cb(OpCode::RL(OpVal::B)),
        0x11 => map_cb(OpCode::RL(OpVal::C)),
        0x12 => map_cb(OpCode::RL(OpVal::D)),
        0x13 => map_cb(OpCode::RL(OpVal::E)),
        0x14 => map_cb(OpCode::RL(OpVal::H)),
        0x15 => map_cb(OpCode::RL(OpVal::L)),
        0x16 => map_cb(OpCode::RL(OpVal::AHL)),
        0x17 => map_cb(OpCode::RL(OpVal::A)),
        0x18 => map_cb(OpCode::RR(OpVal::B)),
        0x19 => map_cb(OpCode::RR(OpVal::C)),
        0x1A => map_cb(OpCode::RR(OpVal::D)),
        0x1B => map_cb(OpCode::RR(OpVal::E)),
        0x1C => map_cb(OpCode::RR(OpVal::H)),
        0x1D => map_cb(OpCode::RR(OpVal::L)),
        0x1E => map_cb(OpCode::RR(OpVal::AHL)),
        0x1F => map_cb(OpCode::RR(OpVal::A)),
        0x20 => map_cb(OpCode::SLA(OpVal::B)),
        0x21 => map_cb(OpCode::SLA(OpVal::C)),
        0x22 => map_cb(OpCode::SLA(OpVal::D)),
        0x23 => map_cb(OpCode::SLA(OpVal::E)),
        0x24 => map_cb(OpCode::SLA(OpVal::H)),
        0x25 => map_cb(OpCode::SLA(OpVal::L)),
        0x26 => map_cb(OpCode::SLA(OpVal::AHL)),
        0x27 => map_cb(OpCode::SLA(OpVal::A)),
        0x28 => map_cb(OpCode::SRA(OpVal::B)),
        0x29 => map_cb(OpCode::SRA(OpVal::C)),
        0x2A => map_cb(OpCode::SRA(OpVal::D)),
        0x2B => map_cb(OpCode::SRA(OpVal::E)),
        0x2C => map_cb(OpCode::SRA(OpVal::H)),
        0x2D => map_cb(OpCode::SRA(OpVal::L)),
        0x2E => map_cb(OpCode::SRA(OpVal::AHL)),
        0x2F => map_cb(OpCode::SRA(OpVal::A)),
        0x30 => map_cb(OpCode::SWAP(OpVal::B)),
        0x31 => map_cb(OpCode::SWAP(OpVal::C)),
        0x32 => map_cb(OpCode::SWAP(OpVal::D)),
        0x33 => map_cb(OpCode::SWAP(OpVal::E)),
        0x34 => map_cb(OpCode::SWAP(OpVal::H)),
        0x35 => map_cb(OpCode::SWAP(OpVal::L)),
        0x36 => map_cb(OpCode::SWAP(OpVal::AHL)),
        0x37 => map_cb(OpCode::SWAP(OpVal::A)),
        0x38 => map_cb(OpCode::SRL(OpVal::B)),
        0x39 => map_cb(OpCode::SRL(OpVal::C)),
        0x3A => map_cb(OpCode::SRL(OpVal::D)),
        0x3B => map_cb(OpCode::SRL(OpVal::E)),
        0x3C => map_cb(OpCode::SRL(OpVal::H)),
        0x3D => map_cb(OpCode::SRL(OpVal::L)),
        0x3E => map_cb(OpCode::SRL(OpVal::AHL)),
        0x3F => map_cb(OpCode::SRL(OpVal::A)),
        0x40 => map_cb(OpCode::BIT(OpBit::B0, OpVal::B)),
        0x41 => map_cb(OpCode::BIT(OpBit::B0, OpVal::C)),
        0x42 => map_cb(OpCode::BIT(OpBit::B0, OpVal::D)),
        0x43 => map_cb(OpCode::BIT(OpBit::B0, OpVal::E)),
        0x44 => map_cb(OpCode::BIT(OpBit::B0, OpVal::H)),
        0x45 => map_cb(OpCode::BIT(OpBit::B0, OpVal::L)),
        0x46 => map_cb(OpCode::BIT(OpBit::B0, OpVal::AHL)),
        0x47 => map_cb(OpCode::BIT(OpBit::B0, OpVal::A)),
        0x48 => map_cb(OpCode::BIT(OpBit::B1, OpVal::B)),
        0x49 => map_cb(OpCode::BIT(OpBit::B1, OpVal::C)),
        0x4A => map_cb(OpCode::BIT(OpBit::B1, OpVal::D)),
        0x4B => map_cb(OpCode::BIT(OpBit::B1, OpVal::E)),
        0x4C => map_cb(OpCode::BIT(OpBit::B1, OpVal::H)),
        0x4D => map_cb(OpCode::BIT(OpBit::B1, OpVal::L)),
        0x4E => map_cb(OpCode::BIT(OpBit::B1, OpVal::AHL)),
        0x4F => map_cb(OpCode::BIT(OpBit::B1, OpVal::A)),
        0x50 => map_cb(OpCode::BIT(OpBit::B2, OpVal::B)),
        0x51 => map_cb(OpCode::BIT(OpBit::B2, OpVal::C)),
        0x52 => map_cb(OpCode::BIT(OpBit::B2, OpVal::D)),
        0x53 => map_cb(OpCode::BIT(OpBit::B2, OpVal::E)),
        0x54 => map_cb(OpCode::BIT(OpBit::B2, OpVal::H)),
        0x55 => map_cb(OpCode::BIT(OpBit::B2, OpVal::L)),
        0x56 => map_cb(OpCode::BIT(OpBit::B2, OpVal::AHL)),
        0x57 => map_cb(OpCode::BIT(OpBit::B2, OpVal::A)),
        0x58 => map_cb(OpCode::BIT(OpBit::B3, OpVal::B)),
        0x59 => map_cb(OpCode::BIT(OpBit::B3, OpVal::C)),
        0x5A => map_cb(OpCode::BIT(OpBit::B3, OpVal::D)),
        0x5B => map_cb(OpCode::BIT(OpBit::B3, OpVal::E)),
        0x5C => map_cb(OpCode::BIT(OpBit::B3, OpVal::H)),
        0x5D => map_cb(OpCode::BIT(OpBit::B3, OpVal::L)),
        0x5E => map_cb(OpCode::BIT(OpBit::B3, OpVal::AHL)),
        0x5F => map_cb(OpCode::BIT(OpBit::B3, OpVal::A)),
        0x60 => map_cb(OpCode::BIT(OpBit::B4, OpVal::B)),
        0x61 => map_cb(OpCode::BIT(OpBit::B4, OpVal::C)),
        0x62 => map_cb(OpCode::BIT(OpBit::B4, OpVal::D)),
        0x63 => map_cb(OpCode::BIT(OpBit::B4, OpVal::E)),
        0x64 => map_cb(OpCode::BIT(OpBit::B4, OpVal::H)),
        0x65 => map_cb(OpCode::BIT(OpBit::B4, OpVal::L)),
        0x66 => map_cb(OpCode::BIT(OpBit::B4, OpVal::AHL)),
        0x67 => map_cb(OpCode::BIT(OpBit::B4, OpVal::A)),
        0x68 => map_cb(OpCode::BIT(OpBit::B5, OpVal::B)),
        0x69 => map_cb(OpCode::BIT(OpBit::B5, OpVal::C)),
        0x6A => map_cb(OpCode::BIT(OpBit::B5, OpVal::D)),
        0x6B => map_cb(OpCode::BIT(OpBit::B5, OpVal::E)),
        0x6C => map_cb(OpCode::BIT(OpBit::B5, OpVal::H)),
        0x6D => map_cb(OpCode::BIT(OpBit::B5, OpVal::L)),
        0x6E => map_cb(OpCode::BIT(OpBit::B5, OpVal::AHL)),
        0x6F => map_cb(OpCode::BIT(OpBit::B5, OpVal::A)),
        0x70 => map_cb(OpCode::BIT(OpBit::B6, OpVal::B)),
        0x71 => map_cb(OpCode::BIT(OpBit::B6, OpVal::C)),
        0x72 => map_cb(OpCode::BIT(OpBit::B6, OpVal::D)),
        0x73 => map_cb(OpCode::BIT(OpBit::B6, OpVal::E)),
        0x74 => map_cb(OpCode::BIT(OpBit::B6, OpVal::H)),
        0x75 => map_cb(OpCode::BIT(OpBit::B6, OpVal::L)),
        0x76 => map_cb(OpCode::BIT(OpBit::B6, OpVal::AHL)),
        0x77 => map_cb(OpCode::BIT(OpBit::B6, OpVal::A)),
        0x78 => map_cb(OpCode::BIT(OpBit::B7, OpVal::B)),
        0x79 => map_cb(OpCode::BIT(OpBit::B7, OpVal::C)),
        0x7A => map_cb(OpCode::BIT(OpBit::B7, OpVal::D)),
        0x7B => map_cb(OpCode::BIT(OpBit::B7, OpVal::E)),
        0x7C => map_cb(OpCode::BIT(OpBit::B7, OpVal::H)),
        0x7D => map_cb(OpCode::BIT(OpBit::B7, OpVal::L)),
        0x7E => map_cb(OpCode::BIT(OpBit::B7, OpVal::AHL)),
        0x7F => map_cb(OpCode::BIT(OpBit::B7, OpVal::A)),
        0x80 => map_cb(OpCode::RES(OpBit::B0, OpVal::B)),
        0x81 => map_cb(OpCode::RES(OpBit::B0, OpVal::C)),
        0x82 => map_cb(OpCode::RES(OpBit::B0, OpVal::D)),
        0x83 => map_cb(OpCode::RES(OpBit::B0, OpVal::E)),
        0x84 => map_cb(OpCode::RES(OpBit::B0, OpVal::H)),
        0x85 => map_cb(OpCode::RES(OpBit::B0, OpVal::L)),
        0x86 => map_cb(OpCode::RES(OpBit::B0, OpVal::AHL)),
        0x87 => map_cb(OpCode::RES(OpBit::B0, OpVal::A)),
        0x88 => map_cb(OpCode::RES(OpBit::B1, OpVal::B)),
        0x89 => map_cb(OpCode::RES(OpBit::B1, OpVal::C)),
        0x8A => map_cb(OpCode::RES(OpBit::B1, OpVal::D)),
        0x8B => map_cb(OpCode::RES(OpBit::B1, OpVal::E)),
        0x8C => map_cb(OpCode::RES(OpBit::B1, OpVal::H)),
        0x8D => map_cb(OpCode::RES(OpBit::B1, OpVal::L)),
        0x8E => map_cb(OpCode::RES(OpBit::B1, OpVal::AHL)),
        0x8F => map_cb(OpCode::RES(OpBit::B1, OpVal::A)),
        0x90 => map_cb(OpCode::RES(OpBit::B2, OpVal::B)),
        0x91 => map_cb(OpCode::RES(OpBit::B2, OpVal::C)),
        0x92 => map_cb(OpCode::RES(OpBit::B2, OpVal::D)),
        0x93 => map_cb(OpCode::RES(OpBit::B2, OpVal::E)),
        0x94 => map_cb(OpCode::RES(OpBit::B2, OpVal::H)),
        0x95 => map_cb(OpCode::RES(OpBit::B2, OpVal::L)),
        0x96 => map_cb(OpCode::RES(OpBit::B2, OpVal::AHL)),
        0x97 => map_cb(OpCode::RES(OpBit::B2, OpVal::A)),
        0x98 => map_cb(OpCode::RES(OpBit::B3, OpVal::B)),
        0x99 => map_cb(OpCode::RES(OpBit::B3, OpVal::C)),
        0x9A => map_cb(OpCode::RES(OpBit::B3, OpVal::D)),
        0x9B => map_cb(OpCode::RES(OpBit::B3, OpVal::E)),
        0x9C => map_cb(OpCode::RES(OpBit::B3, OpVal::H)),
        0x9D => map_cb(OpCode::RES(OpBit::B3, OpVal::L)),
        0x9E => map_cb(OpCode::RES(OpBit::B3, OpVal::AHL)),
        0x9F => map_cb(OpCode::RES(OpBit::B3, OpVal::A)),
        0xA0 => map_cb(OpCode::RES(OpBit::B4, OpVal::B)),
        0xA1 => map_cb(OpCode::RES(OpBit::B4, OpVal::C)),
        0xA2 => map_cb(OpCode::RES(OpBit::B4, OpVal::D)),
        0xA3 => map_cb(OpCode::RES(OpBit::B4, OpVal::E)),
        0xA4 => map_cb(OpCode::RES(OpBit::B4, OpVal::H)),
        0xA5 => map_cb(OpCode::RES(OpBit::B4, OpVal::L)),
        0xA6 => map_cb(OpCode::RES(OpBit::B4, OpVal::AHL)),
        0xA7 => map_cb(OpCode::RES(OpBit::B4, OpVal::A)),
        0xA8 => map_cb(OpCode::RES(OpBit::B5, OpVal::B)),
        0xA9 => map_cb(OpCode::RES(OpBit::B5, OpVal::C)),
        0xAA => map_cb(OpCode::RES(OpBit::B5, OpVal::D)),
        0xAB => map_cb(OpCode::RES(OpBit::B5, OpVal::E)),
        0xAC => map_cb(OpCode::RES(OpBit::B5, OpVal::H)),
        0xAD => map_cb(OpCode::RES(OpBit::B5, OpVal::L)),
        0xAE => map_cb(OpCode::RES(OpBit::B5, OpVal::AHL)),
        0xAF => map_cb(OpCode::RES(OpBit::B5, OpVal::A)),
        0xB0 => map_cb(OpCode::RES(OpBit::B6, OpVal::B)),
        0xB1 => map_cb(OpCode::RES(OpBit::B6, OpVal::C)),
        0xB2 => map_cb(OpCode::RES(OpBit::B6, OpVal::D)),
        0xB3 => map_cb(OpCode::RES(OpBit::B6, OpVal::E)),
        0xB4 => map_cb(OpCode::RES(OpBit::B6, OpVal::H)),
        0xB5 => map_cb(OpCode::RES(OpBit::B6, OpVal::L)),
        0xB6 => map_cb(OpCode::RES(OpBit::B6, OpVal::AHL)),
        0xB7 => map_cb(OpCode::RES(OpBit::B6, OpVal::A)),
        0xB8 => map_cb(OpCode::RES(OpBit::B7, OpVal::B)),
        0xB9 => map_cb(OpCode::RES(OpBit::B7, OpVal::C)),
        0xBA => map_cb(OpCode::RES(OpBit::B7, OpVal::D)),
        0xBB => map_cb(OpCode::RES(OpBit::B7, OpVal::E)),
        0xBC => map_cb(OpCode::RES(OpBit::B7, OpVal::H)),
        0xBD => map_cb(OpCode::RES(OpBit::B7, OpVal::L)),
        0xBE => map_cb(OpCode::RES(OpBit::B7, OpVal::AHL)),
        0xBF => map_cb(OpCode::RES(OpBit::B7, OpVal::A)),
        0xC0 => map_cb(OpCode::SET(OpBit::B0, OpVal::B)),
        0xC1 => map_cb(OpCode::SET(OpBit::B0, OpVal::C)),
        0xC2 => map_cb(OpCode::SET(OpBit::B0, OpVal::D)),
        0xC3 => map_cb(OpCode::SET(OpBit::B0, OpVal::E)),
        0xC4 => map_cb(OpCode::SET(OpBit::B0, OpVal::H)),
        0xC5 => map_cb(OpCode::SET(OpBit::B0, OpVal::L)),
        0xC6 => map_cb(OpCode::SET(OpBit::B0, OpVal::AHL)),
        0xC7 => map_cb(OpCode::SET(OpBit::B0, OpVal::A)),
        0xC8 => map_cb(OpCode::SET(OpBit::B1, OpVal::B)),
        0xC9 => map_cb(OpCode::SET(OpBit::B1, OpVal::C)),
        0xCA => map_cb(OpCode::SET(OpBit::B1, OpVal::D)),
        0xCB => map_cb(OpCode::SET(OpBit::B1, OpVal::E)),
        0xCC => map_cb(OpCode::SET(OpBit::B1, OpVal::H)),
        0xCD => map_cb(OpCode::SET(OpBit::B1, OpVal::L)),
        0xCE => map_cb(OpCode::SET(OpBit::B1, OpVal::AHL)),
        0xCF => map_cb(OpCode::SET(OpBit::B1, OpVal::A)),
        0xD0 => map_cb(OpCode::SET(OpBit::B2, OpVal::B)),
        0xD1 => map_cb(OpCode::SET(OpBit::B2, OpVal::C)),
        0xD2 => map_cb(OpCode::SET(OpBit::B2, OpVal::D)),
        0xD3 => map_cb(OpCode::SET(OpBit::B2, OpVal::E)),
        0xD4 => map_cb(OpCode::SET(OpBit::B2, OpVal::H)),
        0xD5 => map_cb(OpCode::SET(OpBit::B2, OpVal::L)),
        0xD6 => map_cb(OpCode::SET(OpBit::B2, OpVal::AHL)),
        0xD7 => map_cb(OpCode::SET(OpBit::B2, OpVal::A)),
        0xD8 => map_cb(OpCode::SET(OpBit::B3, OpVal::B)),
        0xD9 => map_cb(OpCode::SET(OpBit::B3, OpVal::C)),
        0xDA => map_cb(OpCode::SET(OpBit::B3, OpVal::D)),
        0xDB => map_cb(OpCode::SET(OpBit::B3, OpVal::E)),
        0xDC => map_cb(OpCode::SET(OpBit::B3, OpVal::H)),
        0xDD => map_cb(OpCode::SET(OpBit::B3, OpVal::L)),
        0xDE => map_cb(OpCode::SET(OpBit::B3, OpVal::AHL)),
        0xDF => map_cb(OpCode::SET(OpBit::B3, OpVal::A)),
        0xE0 => map_cb(OpCode::SET(OpBit::B4, OpVal::B)),
        0xE1 => map_cb(OpCode::SET(OpBit::B4, OpVal::C)),
        0xE2 => map_cb(OpCode::SET(OpBit::B4, OpVal::D)),
        0xE3 => map_cb(OpCode::SET(OpBit::B4, OpVal::E)),
        0xE4 => map_cb(OpCode::SET(OpBit::B4, OpVal::H)),
        0xE5 => map_cb(OpCode::SET(OpBit::B4, OpVal::L)),
        0xE6 => map_cb(OpCode::SET(OpBit::B4, OpVal::AHL)),
        0xE7 => map_cb(OpCode::SET(OpBit::B4, OpVal::A)),
        0xE8 => map_cb(OpCode::SET(OpBit::B5, OpVal::B)),
        0xE9 => map_cb(OpCode::SET(OpBit::B5, OpVal::C)),
        0xEA => map_cb(OpCode::SET(OpBit::B5, OpVal::D)),
        0xEB => map_cb(OpCode::SET(OpBit::B5, OpVal::E)),
        0xEC => map_cb(OpCode::SET(OpBit::B5, OpVal::H)),
        0xED => map_cb(OpCode::SET(OpBit::B5, OpVal::L)),
        0xEE => map_cb(OpCode::SET(OpBit::B5, OpVal::AHL)),
        0xEF => map_cb(OpCode::SET(OpBit::B5, OpVal::A)),
        0xF0 => map_cb(OpCode::SET(OpBit::B6, OpVal::B)),
        0xF1 => map_cb(OpCode::SET(OpBit::B6, OpVal::C)),
        0xF2 => map_cb(OpCode::SET(OpBit::B6, OpVal::D)),
        0xF3 => map_cb(OpCode::SET(OpBit::B6, OpVal::E)),
        0xF4 => map_cb(OpCode::SET(OpBit::B6, OpVal::H)),
        0xF5 => map_cb(OpCode::SET(OpBit::B6, OpVal::L)),
        0xF6 => map_cb(OpCode::SET(OpBit::B6, OpVal::AHL)),
        0xF7 => map_cb(OpCode::SET(OpBit::B6, OpVal::A)),
        0xF8 => map_cb(OpCode::SET(OpBit::B7, OpVal::B)),
        0xF9 => map_cb(OpCode::SET(OpBit::B7, OpVal::C)),
        0xFA => map_cb(OpCode::SET(OpBit::B7, OpVal::D)),
        0xFB => map_cb(OpCode::SET(OpBit::B7, OpVal::E)),
        0xFC => map_cb(OpCode::SET(OpBit::B7, OpVal::H)),
        0xFD => map_cb(OpCode::SET(OpBit::B7, OpVal::L)),
        0xFE => map_cb(OpCode::SET(OpBit::B7, OpVal::AHL)),
        0xFF => map_cb(OpCode::SET(OpBit::B7, OpVal::A)),
        unknown => map_cb_unknown(unknown, offset)
    }
}

// Map an operation that takes zero additional bytes.
fn map<T>(value: T) -> Result<(T, usize), String> {
    Ok((value, 0))
}

// Map a cb operation that takes one additional byte.
fn map_cb<T>(value: T) -> Result<(T, usize), String> {
    Ok((value, 1))
}

// Map an operation that takes one additional byte.
fn map_u8<T>(bytes: &[u8], offset: usize, map: fn(u8) -> T) -> Result<(T, usize), String> {
    if bytes.len() >= 1 {
        Ok((map(bytes[0]), 1))
    } else {
        return Err(format!("expected 1 byte at offset {}", offset))
    }
}

// Map an operation that takes two additional bytes.
fn map_u16<T>(bytes: &[u8], offset: usize, map: fn(u16) -> T) -> Result<(T, usize), String> {
    if bytes.len() >= 2 {
        let value = ((bytes[0] as u16) << 8) | (bytes[1] as u16);

        Ok((map(value), 2))
    } else {
        return Err(format!("expected 2 bytes at offset {}", offset))
    }
}

// Maps to an unknown error for a code.
fn map_unknown(code: u8, offset: usize) -> Result<(OpCode, usize), String> {
    Err(format!("unknown instruction 0x{:X?} at offset {}", code, offset))
}

// Maps to an unknown error for a cb prefixed code.
fn map_cb_unknown(code: u8, offset: usize) -> Result<(OpCode, usize), String> {
    Err(format!("unknown cb instruction 0x{:X?} at offset {}", code, offset))
}