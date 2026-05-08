#[derive(Debug)]
pub enum VmErr {
    TypeError,
    StackUnderflow,
    StackOverflow,
}
