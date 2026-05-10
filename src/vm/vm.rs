use crate::vm::{
    op_codes::OpCode, program::Program, stack::Stack, stack_value::StackValue, vm_error::VmErr,
};

type VmResult = Result<(), VmErr>;

pub struct VM {
    stack: Stack<StackValue>,
    program: Program,
    constants: Vec<StackValue>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            program: Program::new(vec![]),
            constants: vec![],
        }
    }
    pub fn execute(&mut self) {
        while self.program.has_next() {
            let op = self.program.take_byte();
            if let Err(e) = OP_TABLE[op as usize](self) {
                eprintln!("{e:?}");
                break;
            }
        }
    }

    pub fn load_program(&mut self, program: &[u8]) {
        self.program = Program::new(program.to_vec());
    }
}

fn nop(vm: &mut VM) -> Result<(), VmErr> {
    vm.program.step(1);
    Ok(())
}

fn illegal_op(_vm: &mut VM) -> VmResult {
    panic!("illegal op");
}

fn add(vm: &mut VM) -> VmResult {
    let (a, b) = vm.stack.pop_2()?;
    vm.stack.push(a + b);
    vm.program.step(1);
    Ok(())
}

fn sub(vm: &mut VM) -> VmResult {
    let (a, b) = vm.stack.pop_2()?;
    vm.stack.push(b - a);
    vm.program.step(1);
    Ok(())
}

fn iconst(vm: &mut VM) -> VmResult {
    let v = vm.program.take_byte();
    vm.stack.push(StackValue::I64(v as i64));
    vm.program.step(1);
    Ok(())
}

fn load_const(vm: &mut VM) -> VmResult {
    let idx = vm.program.take_byte();
    vm.stack.push(vm.constants[idx as usize]);
    Ok(())
}

const OP_TABLE: [fn(&mut VM) -> VmResult; 256] = {
    let mut table = [illegal_op as fn(&mut VM) -> VmResult; 256];
    table[OpCode::Nop as usize] = nop;
    table[OpCode::Add as usize] = add;
    table[OpCode::Sub as usize] = sub;
    table[OpCode::IConst as usize] = iconst;
    table[OpCode::LoadConst as usize] = load_const;
    table
};

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_vm(const1: u8, const2: u8, op: OpCode) -> VM {
        let mut vm = VM::new();
        vm.constants.push(StackValue::I64(10));
        vm.constants.push(StackValue::I64(5));
        vm.load_program(&[
            OpCode::LoadConst as u8,
            const1,
            OpCode::LoadConst as u8,
            const2,
            op as u8,
        ]);
        vm
    }

    #[test]
    fn test_vm_add() {
        let mut vm = setup_vm(0, 1, OpCode::Add);
        vm.execute();
        let r = vm.stack.peek().unwrap();
        assert_eq!(r, StackValue::I64(15));
    }

    #[test]
    fn test_vm_sub() {
        let mut vm = setup_vm(0, 1, OpCode::Sub);
        vm.execute();
        let r = vm.stack.peek().unwrap();
        assert_eq!(r, StackValue::I64(5));
    }
    #[test]
    fn test_vm_should_error() {
        let mut vm = VM::new();
        vm.load_program(&[OpCode::Add as u8]);
        vm.execute();
        let r = vm.stack.peek().unwrap();
        assert_eq!(r, StackValue::I64(5));
    }
}
