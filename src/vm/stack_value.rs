use std::ops::{Add, Sub};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StackValue {
    I64(i64),
    F64(f64),
    Index(usize),
}

impl Default for StackValue {
    fn default() -> Self {
        Self::I64(0)
    }
}

impl Add for StackValue {
    type Output = StackValue;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::I64(a), StackValue::I64(b)) => StackValue::I64(a + b),
            (StackValue::F64(a), StackValue::F64(b)) => StackValue::F64(a + b),
            _ => todo!(),
        }
    }
}

impl Sub for StackValue {
    type Output = StackValue;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (StackValue::I64(a), StackValue::I64(b)) => StackValue::I64(a - b),
            (StackValue::F64(a), StackValue::F64(b)) => StackValue::F64(a - b),
            _ => todo!(),
        }
    }
}
