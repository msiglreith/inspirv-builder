
use std::mem::transmute;
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
use linked_hash_map::LinkedHashMap;
use function::{FuncId, Function};
use cfg;

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
    Vector{ base: Box<Type>, components: u32},
    Matrix{ base: Box<Type>, rows: u32, cols: u32 }, // column major!
    Struct(Vec<Type>),
}

impl Type {
    // TODO: matrix must differ between row and coloumn major
    pub fn alignment(&self) -> usize {
        use self::Type::*;
        match *self {
            Bool => 4,
            Int(bit_width, _) | Float(bit_width) => (7+bit_width as usize)/8,
            Vector { ref base, components } => {
                let base_size = base.size_of();
                let mult = if components == 3 { 4 } else { components } as usize;
                base_size * mult
            }
            Matrix { ref base, rows, cols } => {
                let base_size = base.size_of();
                let vec_align = if rows == 3 { 4 } else { rows } as usize;
                base_size * vec_align as usize * cols as usize
            }

            _ => panic!("Currently unsupported alignment"),
        }
    }

    pub fn size_of(&self) -> usize {
        use self::Type::*;
        match *self {
            Bool => 4,
            Int(bit_width, _) | Float(bit_width) => (7+bit_width as usize)/8,
            Vector { ref base, components } => {
                let base_size = base.size_of();
                base_size * components as usize
            }
            Matrix { ref base, rows, cols } => {
                let base_size = base.size_of();
                let vec_size = if rows == 3 { 4 } else { rows } as usize;
                base_size * vec_size as usize * cols as usize
            }

            _ => panic!("Currently unsupported size_of"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ConstValue {
    Bool(bool),
    I16(i16),
    I32(i32),
    I64(i64),
    Isize(i32),
    U16(u16),
    U32(u32),
    U64(u64),
    Usize(u32),
}

#[derive(Clone, Debug)]
pub enum ConstValueFloat {
    F32(f32),
    F64(f64),
}

#[derive(Clone, Debug)]
pub enum Constant {
    Scalar(ConstValue),
    Float(ConstValueFloat),
    Composite(Type, Vec<Id>),
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
    func_defs: HashMap<FuncId, Function>,
    types: LinkedHashMap<Type, Id>,
    global_vars: HashMap<(String, StorageClass), Variable>,
    consts: HashMap<ConstValue, Id>,
    consts_float: Vec<(ConstValueFloat, Id)>, // floats doesn't support Eq/Hash /shrug
    consts_composite: HashMap<(Type, Vec<Id>), Id>,
    ext_imports: HashMap<String, Id>,

    // debug annotations
    source: Option<(SourceLanguage, LiteralInteger)>,
    id_names: HashMap<Id, String>,
    member_names: HashMap<(Id, u32), String>,

    decorations: HashSet<(Id, Decoration)>,
    decorations_member: HashSet<(Id, u32, Decoration)>,

    cur_id: u32,
}

impl ModuleBuilder {
    pub fn new() -> ModuleBuilder {
        ModuleBuilder {
            memory_model: (
                AddressingModel::AddressingModelLogical,
                MemoryModel::MemoryModelSimple,
            ),
            entry_points: Default::default(),
            func_decls: Default::default(),
            func_defs: Default::default(),
            types: Default::default(),
            global_vars: Default::default(),
            consts: Default::default(),
            consts_float: Default::default(),
            consts_composite: Default::default(),
            source: None,
            id_names: Default::default(),
            member_names: Default::default(),
            decorations: Default::default(),
            decorations_member: Default::default(),
            ext_imports: Default::default(),

            cur_id: 1,
        }
    }

    // TODO: high: complete this!
    pub fn build(&mut self) -> Result<RawModule, BuilderError> {
        // 1. All `OpCapability` instructions
        // NOTE: We retrieve all required capabilities from all the other instructions, so we delay this step
        let mut capabilities = HashSet::new();

        // 2. Optional `OpExtension` instructions (extensions to SPIR-V)

        // 3. Optional `OpExtInstImport` instructions
        let mut instr_ext_import = Vec::new();
        for (name, id) in &self.ext_imports {
            instr_ext_import.push(
                core_instruction::OpExtInstImport(
                    *id,
                    LiteralString(name.clone()),
                ).into()
            );
        }

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
        let mut instr_annotation = Vec::new();
        for &(id, ref decoration) in &self.decorations {
            instr_annotation.push(
                core_instruction::OpDecorate(
                    id,
                    decoration.clone(),
                ).into()
            );
        }

        for &(id, member, ref decoration) in &self.decorations_member {
            instr_annotation.push(
                core_instruction::OpMemberDecorate(
                    id,
                    LiteralInteger(member),
                    decoration.clone(),
                ).into()
            );
        }

        // 9. All type declarations (OpTypeXXX instructions), all constant instructions, and all global variable declarations (all
        //    OpVariable instructions whose Storage Class is not Function)
        // NOTE: Type declarations are defined during the function building step, so we delay this step
        let mut instr_consts = Vec::new();
        for (const_val, id) in self.consts.clone() {
            match const_val {
                ConstValue::Bool(true) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstantTrue(
                            core_instruction::OpConstantTrue(self.define_type(&Type::Bool), id)
                        ))
                    );
                },
                ConstValue::Bool(false) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstantFalse(
                            core_instruction::OpConstantFalse(self.define_type(&Type::Bool), id)
                        ))
                    );
                },
                ConstValue::I16(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(16, true)), id, vec![LiteralInteger(v as u32)])
                        ))
                    );
                },
                ConstValue::I32(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(32, true)), id, vec![LiteralInteger(v as u32)])
                        ))
                    );
                },
                ConstValue::I64(v) => {
                    let v: u64 = unsafe { transmute(v) }; // TODO: low: Is this transmute needed? Not totally sure
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(64, true)), id,
                                vec![
                                    LiteralInteger((v & 0xFFFF) as u32),
                                    LiteralInteger(((v >> 32) & 0xFFFF) as u32),
                                ]
                            )
                        ))
                    )
                },
                ConstValue::Isize(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(32, true)), id, vec![LiteralInteger(v as u32)])
                        ))
                    );
                },
                ConstValue::U16(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(16, false)), id, vec![LiteralInteger(v as u32)])
                        ))
                    );
                },
                ConstValue::U32(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(32, false)), id, vec![LiteralInteger(v)])
                        ))
                    );
                },
                ConstValue::U64(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(64, false)), id,
                                vec![
                                    LiteralInteger((v & 0xFFFF) as u32),
                                    LiteralInteger(((v >> 32) & 0xFFFF) as u32),
                                ]
                            )
                        ))
                    );
                },
                ConstValue::Usize(v) => {
                    instr_consts.push(
                        Instruction::Core(core_instruction::Instruction::OpConstant(
                            core_instruction::OpConstant(self.define_type(&Type::Int(32, false)), id, vec![LiteralInteger(v)])
                        ))
                    );
                },
            }
        }
        for (const_val, id) in self.consts_float.clone() {
            match const_val {
                ConstValueFloat::F32(v) => {
                    let v: u32 = unsafe { transmute(v) };
                    instr_consts.push(
                        core_instruction::OpConstant(
                            self.define_type(&Type::Float(32)),
                            id,
                            vec![LiteralInteger(v)],
                        ).into()
                    )
                },
                ConstValueFloat::F64(v) => {
                    let v: u64 = unsafe { transmute(v) };
                    instr_consts.push(
                        core_instruction::OpConstant(self.define_type(&Type::Float(64)), id,
                            vec![
                                LiteralInteger((v & 0xFFFF) as u32),
                                LiteralInteger(((v >> 32) & 0xFFFF) as u32),
                            ]
                        ).into()
                    )
                },
            }
        }
        for ((ty, const_vals), id) in self.consts_composite.clone() {
            instr_consts.push(
                core_instruction::OpConstantComposite(
                    self.define_type(&ty),
                    id,
                    const_vals,
                ).into()
            )
        }

        let mut instr_global_vars = Vec::new();
        for (name_storage, var) in self.global_vars.clone() {
            let storage = name_storage.1;
            if storage == StorageClass::StorageClassFunction {
                return Err(BuilderError::GlobalFunctionVariable); // TODO: low: move this up to place where the variable is defined
            }

            let ty_id = self.define_type(&Type::Pointer(Box::new(var.ty), storage));
            instr_global_vars.push(
                Instruction::Core(core_instruction::Instruction::OpVariable(
                    core_instruction::OpVariable(ty_id, var.id, storage, None)
                ))
            );
        }

        // 10. All function declarations ("declarations" are functions without a body; there is no forward declaration to a function
        //     with a body)

        // 11. All function definitions (functions with a body)
        let mut instr_funcs = Vec::new();
        for (_, func) in self.func_defs.clone() {
            // Function begin
            let ret_ty = self.define_type(&func.ret_ty);
            let func_ty = self.define_type(&Type::Function(
                Box::new(func.ret_ty),
                func.params.iter().map(
                    |param| Type::Pointer(Box::new(param.ty.clone()), StorageClass::StorageClassFunction) 
                ).collect())
            );

            instr_funcs.push(
                Instruction::Core(core_instruction::Instruction::OpFunction(
                    core_instruction::OpFunction(ret_ty, func.id.0, func.control, func_ty)
                ))
            );

            for parameter in func.params {
                let ty_id = self.define_type(&Type::Pointer(Box::new(parameter.ty), StorageClass::StorageClassFunction));
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
                            core_instruction::OpVariable(ty_id, var.id, StorageClass::StorageClassFunction, None)
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

                if let Some(cfg_structure) = block.cfg_structure {
                    use function::CfgStructure::*;
                    match cfg_structure {
                        Selection(merge) => {
                            instr_funcs.push(core_instruction::OpSelectionMerge(merge, SelectionControlNone).into());
                        }
                        Loop { merge, cont } => {
                            let loop_control = LoopControl {
                                bits: LoopControlNone,
                                values: HashMap::new(),
                            };
                            instr_funcs.push(core_instruction::OpLoopMerge(merge, cont, loop_control).into());
                        }
                    }
                }

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
        // Shouldn't define acutal new types in the builder, only construct the corresponding instructions
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
                    match bit_width {
                        16 => { capabilities.insert(Capability::CapabilityInt16); }
                        64 => { capabilities.insert(Capability::CapabilityInt64); }
                        _  => (),
                    }

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
                    match bit_width {
                        16 => { capabilities.insert(Capability::CapabilityFloat16); }
                        64 => { capabilities.insert(Capability::CapabilityFloat64); }
                        _  => (),
                    }
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
                                self.define_constant(Constant::Scalar(ConstValue::U32(len))),
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

                Type::Vector{base, components} => {
                    if components < 2 {
                        return Err(BuilderError::InvalidVectorComponentCount(components));
                    }

                    match *base {
                        Type::Bool | Type::Int(..) | Type::Float(..) => {}
                        _ => return Err(BuilderError::NonScalarVectorType(*base)),
                    }

                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeVector(
                            core_instruction::OpTypeVector(
                                id,
                                self.define_type(&*base),
                                LiteralInteger(components),
                            )
                        )
                    ));
                }

                Type::Matrix{base, rows, cols} => {
                    if rows < 2 || cols < 2 {
                        return Err(BuilderError::InvalidMatrixDimensions(rows, cols));
                    }

                    match *base {
                        Type::Bool | Type::Int(..) | Type::Float(..) => {}
                        _ => return Err(BuilderError::NonScalarVectorType(*base)),
                    }

                    instr_types.push(
                        core_instruction::OpTypeMatrix(
                            id,
                            self.define_type(&Type::Vector{ base: base, components: rows }),
                            LiteralInteger(cols),
                        ).into()
                    );
                }

                Type::Struct(tys) => {
                    let component_ids = tys.iter().map(|ty| self.define_type(ty)).collect();
                    instr_types.push(Instruction::Core(
                        core_instruction::Instruction::OpTypeStruct(
                            core_instruction::OpTypeStruct(id, component_ids)
                        )
                    ));
                }
            }
        }
 
        // Merge everything together in correct order except capabilities
        let mut instructions = Vec::new();
        instructions.extend(instr_ext_import); // 3.
        instructions.push(instr_memory); // 4.
        instructions.extend(instr_entry); // 5./6.
        instructions.extend(instr_debug); // 7.
        instructions.extend(instr_annotation); // 8.
        instructions.extend(instr_types); // 9.
        instructions.extend(instr_consts); // 9.
        instructions.extend(instr_global_vars); // 9.
        instructions.extend(instr_funcs); // 11. 

        // Retrieve all required capabilities from the constructed instructions
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
        self.id_names.entry(id).or_insert(name.to_string());
    }

    pub fn name_id_member(&mut self, id: Id, member: u32, name: &str) {
        self.member_names.entry((id, member)).or_insert(name.to_string());
    }

    pub fn add_decoration(&mut self, id: Id, decoration: Decoration) {
        self.decorations.insert((id, decoration));
    }

    pub fn add_decoration_member(&mut self, id: Id, member: u32, decoration: Decoration) {
        self.decorations_member.insert((id, member, decoration));
    }

    pub fn import_extension(&mut self, name: &str) -> Id {
        if let Some(id) = self.ext_imports.get(name) {
            return *id;
        }

        let id = Id(self.cur_id);
        self.cur_id += 1;
        self.ext_imports.insert(name.into(), id);
        id
    }

    pub fn define_type(&mut self, ty: &Type) -> Id {
        // Ensure that subtypes of aggregate types are defined BEFORE the actual aggregate type definition
        match *ty {
            Type::Pointer(ref ty, _) => {
                self.define_type(&*ty);
            }
            Type::Function(ref ret_ty, ref params) => {
                self.define_type(&ret_ty);
                for ty in params {
                    self.define_type(&ty);
                }
            }
            Type::Struct(ref tys) => {
                for ty in tys {
                    self.define_type(&ty);
                }
            }
            Type::Vector{ ref base, .. } => {
                self.define_type(&*base);
            }
            Type::Matrix{ ref base, rows, .. } => {
                self.define_type(&Type::Vector{ base: base.clone(), components: rows});
            }
            _ => {}
        }

        if let Some(id) = self.types.get(ty) {
            return *id;
        }

        let id = Id(self.cur_id);
        self.cur_id += 1;
        self.types.insert(ty.clone(), id);
        id
    }

    pub fn define_named_type(&mut self, ty: &Type, name: &str) -> Id {
        let id = self.define_type(ty);
        self.name_id(id, name);
        id
    }

    pub fn define_constant(&mut self, constant: Constant) -> Id {
        match constant {
            Constant::Scalar(c) => {
                let entry = self.consts.entry(c);
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
            Constant::Float(c) => {
                let id = self.alloc_id();
                self.consts_float.push((c, id));
                id
            },
            Constant::Composite(ty, consts) => {
                let entry = self.consts_composite.entry((ty, consts));
                match entry {
                    Entry::Vacant(e) => {
                        let id = Id(self.cur_id);
                        self.cur_id += 1;
                        e.insert(id);
                        id
                    },

                    Entry::Occupied(e) => { *e.get() },
                }
            },
        }
        
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
        self.func_defs.insert(func.id, func);
    }

    pub fn get_function(&self, id: FuncId) -> Option<&Function> {
        self.func_defs.get(&id)
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
    NonScalarVectorType(Type),
    InvalidVectorComponentCount(u32),
    InvalidMatrixDimensions(u32, u32),
}
