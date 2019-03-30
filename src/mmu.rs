use crate::instruction::Instruction;

const ROM0_OFFSET: usize = 0x0000;
const ROMX_OFFSET: usize = 0x4000;
const VRAM_OFFSET: usize = 0x8000;
const SRAM_OFFSET: usize = 0xA000;
const WRAM0_OFFSET: usize = 0xC000;
const WRAMX_OFFSET: usize = 0xD000;
const ECHO_OFFSET: usize = 0xE000;
const OAM_OFFSET: usize = 0xFE00;
const UNUSED_OFFSET: usize = 0xFEA0;
const IO_REG_OFFSET: usize = 0xFF00;
const HRAM_OFFSET: usize = 0xFF80;
const IE_REF_OFFSET: usize = 0xFFFF;

pub struct Mmu {
    mem: Vec<u8>,
    boot: Vec<u8>,
    rom: Vec<u8>,
    boot_complete: bool
}

impl Mmu {
    pub fn new(boot: Vec<u8>, rom: Vec<u8>) -> Mmu {
        Mmu{ mem: vec![0u8; 0xFFFF], boot, rom, boot_complete: false }
    }

    // Fetch an instruction using the given program counter.
    pub fn fetch_instruction(&mut self, pc: u16) -> Instruction {
        let pc_index = pc as usize;

        let instr_bytes = if self.boot_complete {
            &self.mem
        } else {
            &self.boot
        };

        Instruction::decode(instr_bytes, pc_index).unwrap()
    }

    // TODO: Probably need to look in to this more...probably dependent on context
    // Fetch a value at the given address.
    pub fn fetch_value(&self, addr: u16) -> u8 {
        self.mem[addr as usize]
    }

    // TODO: Probably need to look in to this more...probably dependent on context
    // Store a value at the given address.
    pub fn store_value(&mut self, addr: u16, value: u8) {
        self.mem[addr as usize] = value;
    }
}