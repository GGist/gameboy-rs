#![allow(unused)]

use crate::instruction::Instruction;
use crate::cpu::Cpu;
use std::time::{Instant, Duration};
use crate::mmu::Mmu;
use std::thread;

mod cpu;
mod mmu;
mod instruction;

fn main() {
    let boot = include_bytes!("../data/bootrom.gb");
    let rom = include_bytes!("../data/game.gb");

    let mut mmu = Mmu::new(boot.to_vec(), rom.to_vec());
    let mut cpu = Cpu::new();

    // Give the cpu some cycles to start...
    thread::sleep(Duration::from_millis(1000));
    cpu.update_cycles(Instant::now());

    // Start executing instructions
    while cpu.next_instruction(&mut mmu) {}
}
