
use std::collections::{HashSet, HashMap};
use std::collections::hash_map::Entry;
use std::result;
use std::io;
use std::io::Write;
use inspirv;
use inspirv::module::{Header, Generator};
use inspirv::types::{LiteralInteger, LiteralString, Id};
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
    Array(Box<Type>),
    FixedArray(Box<Type>, u32),
    Pointer(Box<Type>, StorageClass),
}

#[derive(Clone, Debug)]
pub enum ConstValue {
    Bool(bool),
    Int(i64), // NOTE: could be more abstract like a `BigInt` but a `i64` is for the moment totally fine
    Uint(u64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub struct EntryPoint {
    execution_modes: HashMap<ExecutionModeKind, ExecutionMode>,
    func_id: FuncId,
    interfaces: Vec<Id>,
}

#[derive(Clone, Debug)]
pub struct Variable {
    id: Id,
    ty: Type,
}

#[derive(Clone, Debug)]
pub struct RawModule {
    header: Header,
    instructions: Vec<Instruction>,
}

impl RawModule {
    pub fn export_binary<W: Write>(&mut self, inner: W) -> io::Result<()> {
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
    entry_points: HashMap<(String, ExecutionModel), EntryPoint>,
    func_decls: Vec<()>,
    func_defs: Vec<Function>,
    types: HashMap<Type, Id>,
    global_vars: HashMap<(String, StorageClass), Variable>,

    // debug annotations
    source: Option<(SourceLanguage, LiteralInteger)>,
    id_names: HashMap<Id, String>,
    member_names: HashMap<(Id, u32), String>,

    cur_id: u32,
}

impl ModuleBuilder {
    pub fn new() -> ModuleBuilder {
        ModuleBuilder {
            memory_model: (
                AddressingModel::AddressingModelLogical,
                MemoryModel::MemoryModelSimple,
            ),
            entry_points: HashMap::new(),
            func_decls: Vec::new(),
            func_defs: Vec::new(),
            types: HashMap::new(),
            global_vars: HashMap::new(),

            source: None,
            id_names: HashMap::new(),
            member_names: HashMap::new(),

            cur_id: 0,
        }
    }

    // TODO: high: complete this!
    pub fn build(&mut self) -> Result<RawModule, BuilderError> {
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
        let mut instr_entry = Vec::new();
        for (name_model, entry_point) in &self.entry_points {
            // entry point instructions
            instr_entry.push(Instruction::Core(core_instruction::Instruction::OpEntryPoint(
                core_instruction::OpEntryPoint(
                    name_model.1,
                    entry_point.func_id.0,
                    LiteralString(name_model.0.clone()),
                    entry_point.interfaces.clone(),
                )
            )));
        }

        for (_, entry_point) in &self.entry_points {
            // execution mode instructions
            for (_, execution_mode) in &entry_point.execution_modes {
                instr_entry.push(Instruction::Core(core_instruction::Instruction::OpExecutionMode(
                    core_instruction::OpExecutionMode(
                        entry_point.func_id.0,
                        execution_mode.clone(),
                    )
                )));
            }
        }

        // 7. These debug instructions, which must be in the following order:
        let mut instr_debug = Vec::new();

        //      a. all OpString, OpSourceExtension, OpSource, and OpSourceContinued, without forward references
        if let Some(source) = self.source {
            instr_debug.push(
                Instruction::Core(core_instruction::Instruction::OpSource(
                    core_instruction::OpSource(source.0, source.1, None, None)
                ))
            );
        }

        //      b. all OpName and all OpMemberName
        for (id, name) in &self.id_names {
            instr_debug.push(
                Instruction::Core(core_instruction::Instruction::OpName(
                    core_instruction::OpName(*id, LiteralString(name.clone()))
                ))
            );
        }

        for (id_member, name) in &self.member_names {
            instr_debug.push(
                Instruction::Core(core_instruction::Instruction::OpMemberName(
                    core_instruction::OpMemberName(id_member.0, LiteralInteger(id_member.1), LiteralString(name.clone()))
                ))
            );
        }

        // 8. All annotation instructionsa:
        //      a. all decoration instructions (OpDecorate, OpMemberDecorate, OpGroupDecorate, OpGroupMemberDecorate,
        //         and OpDecorationGroup)

        // 9. All type declarations (OpTypeXXX instructions), all constant instructions, and all global variable declarations (all
        //    OpVariable instructions whose Storage Class is not Function)
        // NOTE: Type declarations are defined during the function building step, so we delay this step
        let mut instr_global_vars = Vec::new();
        for (name_storage, var) in self.global_vars.clone() {
            let storage = name_storage.1;
            if storage == StorageClass::StorageClassFunction {
                return Err(BuilderError::GlobalFunctionVariable); // TODO: low: move this up to place where the variable is defined
            }

            let ty_id = self.define_type(&Type::Pointer(Box::new(var.ty), storage));
            instr_global_vars.push(
                Instruction::Core(core_instruction::Instruction::OpVariable(
                    core_instruction::OpVariable(var.id, ty_id, storage, None)
                ))
            );
        }

        // 10. All function declarations ("declarations" are functions without a body; there is no forward declaration to a function
        //     with a body)

        // 11. All function definitions (functions with a body)
        let mut instr_funcs = Vec::new();
        for func in self.func_defs.clone() {
            // Function begin
            let ret_ty = self.define_type(&func.ret_ty);
            let func_ty = self.define_type(&Type::Function(Box::new(func.ret_ty), func.params.iter().map(|param| param.ty.clone() ).collect()));

            instr_funcs.push(
                Instruction::Core(core_instruction::Instruction::OpFunction(
                    core_instruction::OpFunction(ret_ty, func.id.0, func.control, func_ty)
                ))
            );

            for parameter in func.params {
                let ty_id = self.define_type(&parameter.ty);
                instr_funcs.push(
                    Instruction::Core(core_instruction::Instruction::OpFunctionParameter(
                        core_instruction::OpFunctionParameter(ty_id, parameter.id)
                    ))
                );
            }

            // variables
            // we add an start block, where all the variables are defined and which jumps to the actual first block of our function
            if func.blocks.len() <= 0 {
                return Err(BuilderError::BlocklessFunction);
            }

            {
                instr_funcs.push(
                    Instruction::Core(core_instruction::Instruction::OpLabel(
                        core_instruction::OpLabel(self.alloc_id())
                    ))
                );

                // generate variable bindings
                for var in func.variables {
                    let ty_id = self.define_type(&Type::Pointer(Box::new(var.ty), StorageClass::StorageClassFunction));
                    instr_funcs.push(
                        Instruction::Core(core_instruction::Instruction::OpVariable(
                            core_instruction::OpVariable(var.id, ty_id, StorageClass::StorageClassFunction, None)
                        ))
                    );
                }

                instr_funcs.push(
                    Instruction::Core(core_instruction::Instruction::OpBranch(
                        core_instruction::OpBranch(func.blocks[0].label)
                    ))
                );
            }
            
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
        let mut instr_types = Vec::new();
        for (ty, id) in self.types.clone() {
            match ty {
                Type::Void => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeVoid(
                            core_instruction::OpTypeVoid(id)
                        )
                    ));
                },

                Type::Bool => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeBool(
                            core_instruction::OpTypeBool(id)
                        )
                    ));
                },

                Type::Int(bit_width, signed) => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeInt(
                            core_instruction::OpTypeInt(
                                id,
                                LiteralInteger(bit_width),
                                LiteralInteger(signed as u32)
                            )
                        )
                    ));
                },

                Type::Float(bit_width) => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeFloat(
                            core_instruction::OpTypeFloat(id, LiteralInteger(bit_width))
                        )
                    ));
                },

                Type::Function(ret_ty, params) => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeFunction(
                            core_instruction::OpTypeFunction(
                                id,
                                self.define_type(&ret_ty),
                                params.iter().map(|ty| { self.define_type(ty) }).collect::<Vec<Id>>(),
                            )
                        )
                    ));
                },

                Type::Array(ty) => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeRuntimeArray(
                            core_instruction::OpTypeRuntimeArray(
                                id,
                                self.define_type(&ty),
                            )
                        )
                    ));
                },

                Type::FixedArray(ty, len) => {
                    if len <= 0 {
                        return Err(BuilderError::ZeroLengthArray);
                    }

                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeArray(
                            core_instruction::OpTypeArray(
                                id,
                                self.define_type(&ty),
                                self.define_constant(Type::Int(32, false), ConstValue::Uint(len as u64)),
                            )
                        )
                    ));
                }

                Type::Pointer(ty, storage_class) => {
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypePointer(
                            core_instruction::OpTypePointer(
                                id,
                                storage_class,
                                self.define_type(&ty),
                            )
                        )
                    ));
                }
            }
        }
 
        // Merge everything together in correct order except capabilities
        let mut instructions = Vec::new();
        instructions.push(instr_memory); // 4.
        instructions.extend(instr_entry); // 5./6.
        instructions.extend(instr_debug); // 7.
        instructions.extend(instr_types); // 9.
        instructions.extend(instr_funcs); // 11.

        // Retrieve all required capabilities from the constructed instructions
        let mut capabilities = HashSet::new();
        for instr in &instructions {
            capabilities.extend(instr.capabilities());
        }
        let mut instr_capabilites = capabilities.iter().map(|capability| {
                Instruction::Core(core_instruction::Instruction::OpCapability(
                    core_instruction::OpCapability(*capability)
                ))
        }).collect::<Vec<Instruction>>();

        instr_capabilites.extend(instructions); // 1.
        let instructions = instr_capabilites;

        Ok(RawModule {
            header: Header {
                magic_number: inspirv::core::SPIRV_MAGIC,
                version: inspirv::core::SPIRV_VERSION,
                generator: Generator::Unknown(INSPIRV_BUILDER_ID),
                bound: self.cur_id,
            },
            instructions: instructions,
        })
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

    pub fn with_source(&mut self, source_lang: SourceLanguage, language_version: u32) {
        self.source = Some((source_lang, LiteralInteger(language_version)));
    }

    pub fn name_id(&mut self, id: Id, name: &str) {
        {
            if !self.id_names.contains_key(&id) {
                println!("{:?}", (id, name));
            }
            
        }
        self.id_names.entry(id).or_insert(name.to_string());
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

    pub fn define_named_type(&mut self, ty: &Type, name: &str) -> Id {
        let id = self.define_type(ty);
        self.name_id(id, name);
        id
    }

    // TODO: Not totally happy with this function atm as it's pretty unsafe
    pub fn define_constant(&mut self, ty: Type, constant: ConstValue) -> Id {
        unimplemented!()
    }

    // TODO: do we need function declarations at all?
    pub fn declare_function(&mut self, name: &str, ) -> FuncId {
        unimplemented!()
    }

    pub fn define_function(&mut self) -> Function {
        let id = FuncId(self.alloc_id());
        Function {
            id: id,
            params: Vec::new(),
            ret_ty: Type::Void,
            interfaces: Vec::new(),
            variables: Vec::new(),
            control: FunctionControlNone,
            blocks: Vec::new(),
        }
    }

    pub fn define_function_named(&mut self, name: &str) -> Function {
        let func = self.define_function();
        self.name_id(func.id.0, name);
        func
    }

    pub fn push_function(&mut self, func: Function) {
        self.func_defs.push(func);
    }

    pub fn define_variable(&mut self, name: &str, ty: Type, storage: StorageClass) -> Id {  
        let id = {
            let entry = self.global_vars.entry((name.to_string(), storage));
            match entry {
                Entry::Vacant(e) => {
                    let id = Id(self.cur_id);
                    self.cur_id += 1;
                    e.insert(Variable {
                        id: id,
                        ty: ty,
                    });

                    id
                },

                Entry::Occupied(e) => { e.get().id },
            }
        };
        self.name_id(id, name);
        id
    }

    pub fn define_entry_point(
        &mut self,
        name: &str,
        execution_model: ExecutionModel,
        execution_modes: HashMap<ExecutionModeKind, ExecutionMode>,
        interfaces: Vec<Id>,
    ) -> result::Result<Function, BuilderError> {
        let function = self.define_function_named(name);
        let entry_point = EntryPoint {
            execution_modes: execution_modes,
            func_id: function.id,
            interfaces: interfaces,
        };

        if self.entry_points.contains_key(&(name.to_string(), execution_model)) {
            return Err(BuilderError::EntryPointAlreadyDefined(name.to_string(), execution_model));
        }

        self.entry_points.insert((name.to_string(), execution_model), entry_point);
        Ok(function)
    }
}

// https://doc.rust-lang.org/book/error-handling.html#composing-custom-error-types
// TODO: Could need fome love <3
#[derive(Debug)]
pub enum BuilderError {
    ZeroLengthArray,
    BlocklessFunction,
    EntryPointAlreadyDefined(String, ExecutionModel),
    GlobalFunctionVariable,
}
