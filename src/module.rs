
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use std::io::Write;
use inspirv;
use inspirv::module::{Header, Generator};
use inspirv::types::Id;
use inspirv::core::enumeration::*;

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
    instructions: Vec<inspirv::instruction::Instruction>,
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
    memory_model: (MemoryModel, AddressingModel),
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
                MemoryModel::MemoryModelSimple, // TODO: requires `Shader` capability
                AddressingModel::AddressingModelLogical,
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
        RawModule {
            header: Header {
                magic_number: inspirv::core::SPIRV_MAGIC,
                version: inspirv::core::SPIRV_VERSION,
                generator: Generator::Unknown(INSPIRV_BUILDER_ID),
                bound: self.cur_id,
            },
            instructions: Vec::new(),
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
