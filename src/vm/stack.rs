use crate::vm::vm_error::VmErr;

const STACK_SIZE: usize = 1024;
pub struct Stack<T> {
    inner: [T; STACK_SIZE],
    sp: isize,
}

impl<T: Copy + Default> Stack<T> {
    pub fn new() -> Self {
        Self {
            inner: [T::default(); STACK_SIZE],
            sp: -1,
        }
    }
    pub fn push(&mut self, value: T) {
        self.sp += 1;
        assert!((self.sp as usize) < STACK_SIZE, "STACK OWERFLOW");
        self.inner[self.sp as usize] = value;
    }

    pub fn pop(&mut self) -> Result<T, VmErr> {
        let a = self
            .inner
            .get(self.sp as usize)
            .ok_or(VmErr::StackUnderflow)
            .copied()?;
        let v = self.inner[self.sp as usize];
        self.sp -= 1;
        assert!(self.sp >= -1, "STACK UNDERFLOW");
        Ok(v)
    }

    pub fn pop_2(&mut self) -> Result<(T, T), VmErr> {
        let a = self.pop()?;
        let b = self.pop()?;
        Ok((a, b))
    }

    pub fn peek(&self) -> Result<T, VmErr> {
        self.inner
            .get(self.sp as usize)
            .copied()
            .ok_or(VmErr::StackUnderflow)
    }

    pub fn len(&self) -> usize {
        self.sp as usize + 1
    }
}
