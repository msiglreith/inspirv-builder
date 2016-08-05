
use inspirv::types::Id;
use inspirv::instruction::{BranchInstruction, Instruction};
use inspirv::core::enumeration::FunctionControl;
use module::Type;

#[derive(Copy, Clone, Debug)]
pub struct FuncId(pub Id);

pub struct FunctionDecl {
    pub params: Vec<()>,
    pub ret_ty: (),
}

#[derive(Clone, Debug)]
pub struct Block {
    pub label: Id,
    pub branch_instr: Option<BranchInstruction>, // needs to be set on build
    pub instructions: Vec<Instruction>,
}

#[derive(Clone, Debug)]
pub struct Function {
    pub id: FuncId,
    pub params: Vec<Type>,
    pub ret_ty: Type,
    pub variables: Vec<()>,
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
