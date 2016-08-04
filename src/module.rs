
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use std::io::Write;
use inspirv;
use inspirv::module::{Header, Generator};
use inspirv::types::{LiteralInteger, Id};
use inspirv::instruction::Instruction;
use inspirv::core::enumeration::*;
use inspirv::core::instruction as core_instruction;

use function::{FuncId, Function};

const INSPIRV_BUILDER_ID: u32 = 0xCC; // TODO: might be able to get an official value in the future (:?

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Void,
    Bool,
    Int(u32, bool), // bit-width, signed
    Float(u32), // bit-width, following IEEE 754 standard
    Function,
}

pub struct EntryPoint {
    name: String,
    execution_model: ExecutionModel,
    execution_mode: ExecutionMode,
    // TODO: function id
    // TODO: interfaces
}

pub struct RawModule {
    header: Header,
    instructions: Vec<Instruction>,
}

impl RawModule {
    pub fn export_binary<W: Write>(&mut self, inner: W) {
        let mut writer = inspirv::write_binary::WriterBinary::new(inner);
        writer.write_header(self.header);
        for instr in &self.instructions {
            writer.write_instruction(&instr);
        }
    }
}

// Logical layout of a SPIR-V module (Specification 1.1, Section 2.4)
pub struct ModuleBuilder {
    capabilites: HashSet<Capability>,
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
            capabilites: HashSet::new(),
            memory_model: (
                AddressingModel::AddressingModelLogical,
                MemoryModel::MemoryModelSimple, // TODO: requires `Shader` capability
            ),
            entry_points: Vec::new(),
            func_decls: Vec::new(),
            func_defs: Vec::new(),
            types: HashMap::new(),

            cur_id: 0,
        }
    }

    // TODO:
    pub fn build(&self) -> RawModule {
        let mut instructions = Vec::new();

        // 1. All `OpCapability` instructions
        for capability in &self.capabilites {
            instructions.push(Instruction::Core(
                core_instruction::Instruction::OpCapability(
                    core_instruction::OpCapability(*capability)
                )
            ));
        }

        // 2. Optional `OpExtension` instructions (extensions to SPIR-V)

        // 3. Optional `OpExtInstImport` instructions

        // 4. The single required `OpMemoryModel` instruction
        instructions.push(Instruction::Core(
            core_instruction::Instruction::OpMemoryModel(
                core_instruction::OpMemoryModel(self.memory_model.0, self.memory_model.1)
            )
        ));

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
        for (ty, id) in &self.types {
            instructions.push(
                match *ty {
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

                    Type::Function => {
                        unimplemented!()
                    },
                }
            );
        }

        // 10. All function declarations ("declarations" are functions without a body; there is no forward declaration to a function
        //     with a body)

        // 11. All function definitions (functions with a body)

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

    pub fn define_type(&mut self, ty: Type) -> Id {
        let entry = self.types.entry(ty);
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
        self.define_type(Type::Void)
    }

    pub fn define_bool(&mut self) -> Id {
        self.define_type(Type::Bool)
    }

    pub fn define_u8(&mut self) -> Id {
        self.define_type(Type::Int(8, false))
    }

    pub fn define_u16(&mut self) -> Id {
        self.define_type(Type::Int(16, false))
    }

    pub fn define_u32(&mut self) -> Id {
        self.define_type(Type::Int(32, false))
    }

    pub fn define_u64(&mut self) -> Id {
        self.define_type(Type::Int(64, false))
    }

    pub fn define_i8(&mut self) -> Id {
        self.define_type(Type::Int(8, true))
    }

    pub fn define_i16(&mut self) -> Id {
        self.define_type(Type::Int(16, true))
    }

    pub fn define_i32(&mut self) -> Id {
        self.define_type(Type::Int(32, true))
    }

    pub fn define_i64(&mut self) -> Id {
        self.define_type(Type::Int(64, true))
    }

    pub fn define_f32(&mut self) -> Id {
        self.define_type(Type::Float(32))
    }

    pub fn define_f64(&mut self) -> Id {
        self.define_type(Type::Float(64))
    }

    // TODO: do we need function declarations at all?
    pub fn declare_function(&mut self, name: &str, ) -> FuncId {
        unimplemented!()
    }

    pub fn define_function(&mut self) -> (FuncId, Function) {
        unimplemented!()
    }
}
