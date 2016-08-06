
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use std::io::{Result, Write};
use inspirv;
use inspirv::module::{Header, Generator};
use inspirv::types::{LiteralInteger, Id};
use inspirv::instruction::{Instruction, InstructionExt};
use inspirv::core::enumeration::*;
use inspirv::core::instruction as core_instruction;

use function::{FuncId, Function};

const INSPIRV_BUILDER_ID: u32 = 0xCC; // TODO: might be able to get an official value in the future (:?

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    Bool,
    Int(u32, bool), // bit-width, signed
    Float(u32), // bit-width, following IEEE 754 standard
    Function(Box<Type>, Vec<Type>),
}

pub struct EntryPoint {
    name: String,
    execution_model: ExecutionModel,
    execution_mode: ExecutionMode,
    func_id: FuncId,
    // TODO: interfaces
}

pub struct RawModule {
    header: Header,
    instructions: Vec<Instruction>,
}

impl RawModule {
    pub fn export_binary<W: Write>(&mut self, inner: W) -> Result<()> {
        let mut writer = inspirv::write_binary::WriterBinary::new(inner);
        try!(writer.write_header(self.header));
        for instr in &self.instructions {
            try!(writer.write_instruction(&instr));
        }

        Ok(())
    }
}

// Logical layout of a SPIR-V module (Specification 1.1, Section 2.4)
pub struct ModuleBuilder {
    memory_model: (AddressingModel, MemoryModel),
    entry_points: Vec<EntryPoint>,
    func_decls: Vec<()>,
    func_defs: Vec<Function>,
    types: HashMap<Type, Id>,

    cur_id: u32,
}

impl ModuleBuilder {
    pub fn new() -> ModuleBuilder {
        ModuleBuilder {
            memory_model: (
                AddressingModel::AddressingModelLogical,
                MemoryModel::MemoryModelSimple,
            ),
            entry_points: Vec::new(),
            func_decls: Vec::new(),
            func_defs: Vec::new(),
            types: HashMap::new(),

            cur_id: 0,
        }
    }

    // TODO:
    pub fn build(&mut self) -> RawModule {
        // TODO: mid-high: generate capabilites from the instructions we generate below!
        // 1. All `OpCapability` instructions
        // NOTE: We retrieve all required capabilities from all the other instructions, so we delay this step

        // 2. Optional `OpExtension` instructions (extensions to SPIR-V)

        // 3. Optional `OpExtInstImport` instructions

        // 4. The single required `OpMemoryModel` instruction
        let instr_memory = Instruction::Core(
            core_instruction::Instruction::OpMemoryModel(
                core_instruction::OpMemoryModel(self.memory_model.0, self.memory_model.1)
            )
        );

        // 5. All entry point declarations, using `OpEntryPoint`

        // 6. All execution mode declarations, using `OpExecutionMode`

        // 7. These debug instructions, which must be in the following order:
        //      a. all OpString, OpSourceExtension, OpSource, and OpSourceContinued, without forward references
        //      b. all OpName and all OpMemberName

        // 8. All annotation instructionsa:
        //      a. all decoration instructions (OpDecorate, OpMemberDecorate, OpGroupDecorate, OpGroupMemberDecorate,
        //         and OpDecorationGroup)

        // 9. All type declarations (OpTypeXXX instructions), all constant instructions, and all global variable declarations (all
        //    OpVariable instructions whose Storage Class is not Function)
        // NOTE: Type declarations are defined during the function building step, so we delay this step

        // 10. All function declarations ("declarations" are functions without a body; there is no forward declaration to a function
        //     with a body)

        // 11. All function definitions (functions with a body)
        let mut instr_funcs = Vec::new();
        for func in self.func_defs.clone() {
            // Function begin
            let ret_ty = self.define_type(&func.ret_ty);
            let func_ty = self.define_type(&Type::Function(Box::new(func.ret_ty), func.params.clone()));

            instr_funcs.push(
                Instruction::Core(core_instruction::Instruction::OpFunction(
                    core_instruction::OpFunction(ret_ty, func.id.0, func.control, func_ty)
                ))
            );

            for parameter in func.params {
                let id = self.define_type(&parameter);
                let ty_id = self.define_type(&parameter);
                instr_funcs.push(
                    Instruction::Core(core_instruction::Instruction::OpFunctionParameter(
                        core_instruction::OpFunctionParameter(ty_id, id)
                    ))
                );
            }

            // TODO: variables
            
            for block in func.blocks {
                // A block always starts with an `OpLabel` instruction
                instr_funcs.push(
                    Instruction::Core(core_instruction::Instruction::OpLabel(
                        core_instruction::OpLabel(block.label)
                    ))
                );

                instr_funcs.extend(block.instructions);

                // A block always ends with a branch instruction
                instr_funcs.push(Instruction::from(block.branch_instr.unwrap()));
            }

            // Function end
            instr_funcs.push(
                Instruction::Core(core_instruction::Instruction::OpFunctionEnd(
                    core_instruction::OpFunctionEnd
                ))
            );
        }

        // Define all required types as SPIR-V instructions
        let instr_types = self.types.clone().iter().map(|(ty, id)| {
            match ty.clone() {
                Type::Void => {
                    Instruction::Core(
                        core_instruction::Instruction::OpTypeVoid(
                            core_instruction::OpTypeVoid(*id)
                        )
                    )
                },

                Type::Bool => {
                    Instruction::Core(
                        core_instruction::Instruction::OpTypeBool(
                            core_instruction::OpTypeBool(*id)
                        )
                    )
                },

                Type::Int(bit_width, signed) => {
                    Instruction::Core(
                        core_instruction::Instruction::OpTypeInt(
                            core_instruction::OpTypeInt(
                                *id,
                                LiteralInteger(bit_width),
                                LiteralInteger(signed as u32)
                            )
                        )
                    )
                },

                Type::Float(bit_width) => {
                    Instruction::Core(
                        core_instruction::Instruction::OpTypeFloat(
                            core_instruction::OpTypeFloat(*id, LiteralInteger(bit_width))
                        )
                    )
                },

                Type::Function(ret_ty, params) => {
                    Instruction::Core(
                        core_instruction::Instruction::OpTypeFunction(
                            core_instruction::OpTypeFunction(
                                *id,
                                self.define_type(&ret_ty),
                                params.iter().map(|ty| { self.define_type(ty) }).collect::<Vec<Id>>(),
                            )
                        )
                    )
                },
            }
        }).collect::<Vec<Instruction>>();

        // TODO:
        // Retrieve all required capabilities from the constructed instructions
        

        // Merge everything together in correct order except capabilities
        let mut instructions = Vec::new();
        instructions.push(instr_memory);
        instructions.extend(instr_types);
        instructions.extend(instr_funcs);

        let mut capabilities = HashSet::new();
        for instr in &instructions {
            capabilities.extend(instr.capabilities());
        }
        let mut instr_capabilites = capabilities.iter().map(|capability| {
                Instruction::Core(core_instruction::Instruction::OpCapability(
                    core_instruction::OpCapability(*capability)
                ))
        }).collect::<Vec<Instruction>>();

        instr_capabilites.extend(instructions);
        let instructions = instr_capabilites;

        RawModule {
            header: Header {
                magic_number: inspirv::core::SPIRV_MAGIC,
                version: inspirv::core::SPIRV_VERSION,
                generator: Generator::Unknown(INSPIRV_BUILDER_ID),
                bound: self.cur_id,
            },
            instructions: instructions,
        }
    }

    pub fn alloc_id(&mut self) -> Id {
        let id = Id(self.cur_id);
        self.cur_id += 1;
        id
    }

    pub fn with_memory_model(&mut self, memory_model: MemoryModel) {
        self.memory_model.1 = memory_model;
    }

    pub fn with_addressing_model(&mut self, addressing_model: AddressingModel) {
        self.memory_model.0 = addressing_model;
    }

    pub fn define_type(&mut self, ty: &Type) -> Id {
        let entry = self.types.entry(ty.clone());
        match entry {
            Entry::Vacant(e) => {
                let id = Id(self.cur_id);
                self.cur_id += 1;
                e.insert(id);
                id
            },

            Entry::Occupied(e) => { *e.get() },
        }
    }

    pub fn define_void(&mut self) -> Id {
        self.define_type(&Type::Void)
    }

    pub fn define_bool(&mut self) -> Id {
        self.define_type(&Type::Bool)
    }

    pub fn define_u8(&mut self) -> Id {
        self.define_type(&Type::Int(8, false))
    }

    pub fn define_u16(&mut self) -> Id {
        self.define_type(&Type::Int(16, false))
    }

    pub fn define_u32(&mut self) -> Id {
        self.define_type(&Type::Int(32, false))
    }

    pub fn define_u64(&mut self) -> Id {
        self.define_type(&Type::Int(64, false))
    }

    pub fn define_i8(&mut self) -> Id {
        self.define_type(&Type::Int(8, true))
    }

    pub fn define_i16(&mut self) -> Id {
        self.define_type(&Type::Int(16, true))
    }

    pub fn define_i32(&mut self) -> Id {
        self.define_type(&Type::Int(32, true))
    }

    pub fn define_i64(&mut self) -> Id {
        self.define_type(&Type::Int(64, true))
    }

    pub fn define_f32(&mut self) -> Id {
        self.define_type(&Type::Float(32))
    }

    pub fn define_f64(&mut self) -> Id {
        self.define_type(&Type::Float(64))
    }

    // TODO: do we need function declarations at all?
    pub fn declare_function(&mut self, name: &str, ) -> FuncId {
        unimplemented!()
    }

    // TODO: interface
    pub fn define_function(&mut self) -> Function {
        let id = FuncId(self.alloc_id());
        Function {
            id: id,
            params: Vec::new(),
            ret_ty: Type::Void,
            variables: Vec::new(),
            control: FunctionControlNone,
            blocks: Vec::new(),
        }
    }

    pub fn push_function(&mut self, func: Function) {
        self.func_defs.push(func);
    }

    pub fn define_entry_point(&mut self) {
        unimplemented!()
    }
}
