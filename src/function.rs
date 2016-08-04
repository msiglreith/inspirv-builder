
use inspirv::types::Id;
use inspirv::instruction::Instruction;

pub struct FuncId(pub Id);

pub struct FunctionDecl {
    params: Vec<()>,
    ret_ty: (),
}

pub struct Function {
    params: Vec<()>,
    ret_ty: (),
    instructions: Vec<Instruction>,
}