#[repr(u8)]
#[derive(Debug)]
pub enum OpCode {
    Nop,
    Halt,

    Add = 0x10,
    Sub,
    Mul,
    Div,
    Mod,

    IConst,
    FConst,
    LoadConst,
}
