
use inspirv::types::{Id, LiteralInteger};
use inspirv::instruction::{BranchInstruction, Instruction};
use inspirv::core::enumeration::FunctionControl;
use inspirv::core::instruction::OpExtInst;
use inspirv::glsl::instruction as glsl;
use module::Type;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub Id);

pub struct FunctionDecl {
    pub params: Vec<Type>,
    pub ret_ty: Type,
}

#[derive(Clone, Debug)]
pub struct Block {
    pub label: Id,
    pub branch_instr: Option<BranchInstruction>, // needs to be set on build
    pub instructions: Vec<Instruction>, // some checks to avoid illegal instructions might be interesting for the future
}

impl Block {
    pub fn emit_instruction<T: Into<Instruction>>(&mut self, instruction: T) {
        self.instructions.push(instruction.into());
    }

    // TODO: automatically import GLSL extension on call
    pub fn emit_glsl_instruction(&mut self, ext: Id, op: glsl::OpCode, result_id: Id, result_ty: Id, operands: Vec<Id>) {
        self.emit_instruction(OpExtInst(result_ty, result_id, ext, LiteralInteger(op as u32), operands));
    }
}

#[derive(Clone, Debug)]
pub struct LocalVar {
    pub id: Id,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Argument {
    pub id: Id,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Interface {
    pub id: Id,
    pub ty: Type,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: FuncId,
    pub params: Vec<Argument>,
    pub ret_ty: Type,
    pub interfaces: Vec<Interface>,
    pub variables: Vec<LocalVar>,
    pub control: FunctionControl,
    pub blocks: Vec<Block>,
}

impl Function {
    pub fn add_block(&mut self, label: Id) -> &mut Block {
        let block = Block {
            label: label,
            branch_instr: None,
            instructions: Vec::new(),
        };

        self.blocks.push(block);
        self.blocks.last_mut().unwrap()
    }
}
