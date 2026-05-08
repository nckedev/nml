pub struct Program {
    inner: Vec<u8>,
    pc: usize,
}

impl Program {
    pub fn new(bytes: Vec<u8>) -> Self {
        Self {
            inner: bytes,
            pc: 0,
        }
    }

    pub fn take_byte(&mut self) -> u8 {
        assert!(self.pc < self.inner.len(), "Program counter out of bounds");
        let v = self.inner[self.pc];
        self.pc += 1;
        v
    }

    pub fn has_next(&self) -> bool {
        self.pc < self.inner.len()
    }

    pub fn step(&mut self, steps: usize) {
        self.pc += steps;
    }

    pub fn print(&self) {}
}
