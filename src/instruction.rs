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
    BC,
    DE,
    HL,
    SP,
    PC,
    D16(u16)
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
    LDL(OpVal, OpVal),
    LDC(OpVal),
    LDHLI(OpVal),
    LDHLD(OpVal),
    LDHL(OpVal),
    LDLH(OpVal, OpVal),
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
    SBC(OpVal, OpVal),
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
            let code = bytes[0];
            let offset = curr_offset;

            bytes = &bytes[1..];
            curr_offset += 1;

            let (code, size) = decode(code, bytes, curr_offset)?;
            bytes = &bytes[size..];
            curr_offset += size;

            instructions.push(Instruction{ code, offset });
        }

        return Ok(instructions)
    }
}

fn decode(code: u8, bytes: &[u8], offset: usize) -> Result<(OpCode, usize), String> {
    match code {
        0x00 => map(OpCode::NOP),
        0x01 => map_u16(bytes, offset, |direct| {
            OpCode::LDL(OpVal::BC, OpVal::D16(direct))
        }),
        0x02 => map(OpCode::LDL(OpVal::BC, OpVal::A)),
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
        0x0A => map(OpCode::LDL(OpVal::A, OpVal::BC)),
        0x0B =>

        0xC9 => map(OpCode::RET(Option::None)),
        default => Err(format!("unknown instruction 0x{:X?} at offset {}", default, offset))
    }
}

fn map<T>(value: T) -> Result<(T, usize), String> {
    Ok((value, 0))
}

fn map_u8<T>(bytes: &[u8], offset: usize, map: fn(u8) -> T) -> Result<(T, usize), String> {
    if bytes.len() >= 1 {
        Ok((map(bytes[0]), 1))
    } else {
        return Err(format!("expected 1 byte at offset {}", offset))
    }
}

fn map_u16<T>(bytes: &[u8], offset: usize, map: fn(u16) -> T) -> Result<(T, usize), String> {
    if bytes.len() >= 2 {
        let value = ((bytes[0] as u16) << 8) | (bytes[1] as u16);

        Ok((map(value), 2))
    } else {
        return Err(format!("expected 2 bytes at offset {}", offset))
    }
}