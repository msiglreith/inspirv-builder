
use petgraph::{Graph};
use function::{CfgStructure, Function};
use inspirv::instruction::{BranchInstruction};
use inspirv::types::Id;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct BasicBlock {
    label: Id,
    cfg_structure: Option<CfgStructure>,
}

pub fn build_graph(function: &Function) -> Graph<BasicBlock, ()> {
    let mut graph = Graph::new();
    let mut label_lut = HashMap::new();

    // build up generated control flow graph
    // first there where nodes ..
    for block in &function.blocks {
        let bb = BasicBlock {
            label: block.label,
            cfg_structure: None,
        };
        let node = graph.add_node(bb);
        label_lut.insert(block.label, node);
    }

    // .. then the first connections established ..
    for block in &function.blocks {
        let branch_instr = block.branch_instr.clone().expect("Missing branch instruction");
        use inspirv::instruction::BranchInstruction::*;
        match branch_instr {
            Branch(instr) => {
                graph.add_edge(*label_lut.get(&block.label).unwrap(), *label_lut.get(&instr.0).unwrap(), ());
            }
            BranchConditional(instr) => {
                graph.add_edge(*label_lut.get(&block.label).unwrap(), *label_lut.get(&instr.1).unwrap(), ()); // true
                graph.add_edge(*label_lut.get(&block.label).unwrap(), *label_lut.get(&instr.2).unwrap(), ()); // false
            }
            Switch(instr) => {
                // TODO:
            }
            Kill(_) |
            Return(_) |
            ReturnValue(_) |
            Unreachable(_) => { }
        }
    }

    graph
}