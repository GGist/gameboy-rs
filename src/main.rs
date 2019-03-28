use crate::instruction::Instruction;

mod instruction;

fn main() {
    let rom = include_bytes!("../data/test.gb");
    let instructions = Instruction::from_bytes(rom);

    println!("{:?}", instructions);
}
