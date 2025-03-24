use super::common::{X86Arg, X86Instr, X86Program};
use petgraph::{graph::NodeIndex, visit::IntoNodeReferences};
use std::collections::{HashMap, HashSet};

const AVAILABLE_REGISTERS: &[&str] = &["rdx", "rcx", "rsi", "rdi", "r8", "r9", "r10", "rbx", "r12", "r13", "r14"];

/// The program is currently in pseudo x86 because it has variables.
/// In this pass we remove variables, replacing them with either
/// registers or stack references.
pub fn allocate_registers(program: &mut X86Program) {
    for (label, block) in &mut program.blocks {
        let live_after_sets = ul_block(block);
        let mut interference_graph = InterferenceGraph::new();
        build_interference_graph_block(&mut interference_graph, block, &live_after_sets);
        let coloring = disjoint_coloring(&interference_graph);
        let variable_homes = assign_homes(&coloring);

        for instr in block {
            instr.transform_args(|arg| {
                use X86Arg::*;
                if let Var(name) = arg {
                    *arg = variable_homes[name].clone();
                }
            })
        }
    }
}

fn assign_homes(coloring: &HashMap<&str, Color>) -> HashMap<String, X86Arg> {
    let mut vars: Vec<(Color, &str)> = coloring
        .iter()
        .map(|(name, color)| (*color, *name))
        .collect();
    vars.sort_by_key(|x| x.0);
    let vars: Vec<&str> = vars.into_iter().map(|x| x.1).collect();

    let reg_homes_iter = AVAILABLE_REGISTERS
        .iter()
        .map(|reg_name| X86Arg::Reg(reg_name.to_string()));
    let stack_homes_iter = (0..).map(|offset| X86Arg::Deref("rbp".to_string(), (offset + 1) * -8));
    let available_homes = reg_homes_iter.chain(stack_homes_iter);

    HashMap::from_iter(vars.into_iter().map(|x| x.to_string()).zip(available_homes))
}

fn disjoint_coloring<'a>(graph: &InterferenceGraph<'a>) -> HashMap<&'a str, Color> {
    let mut result = HashMap::<&str, Color>::new();
    // Each node's neighboring colors
    let mut saturation_sets = HashMap::<&str, HashSet<Color>>::new();

    let mut variables = HashSet::<(NodeIndex, &str)>::new();
    for (idx, name) in graph.node_references() {
        variables.insert((idx, name));
        saturation_sets.insert(name, HashSet::new());
    }

    loop {
        let Some(&next) = variables
            .iter()
            .min_by_key(|(idx, name)| saturation_sets[*name].len())
        else {
            break;
        };
        let (idx, name) = next;
        variables.remove(&next);

        let color = (0..).find(|c| !saturation_sets[name].contains(c)).unwrap();
        result.insert(name, color);

        for neighbor in graph.neighbors(idx) {
            let neighbor = graph.node_weight(neighbor).unwrap();
            saturation_sets.get_mut(neighbor).unwrap().insert(color);
        }
    }

    result
}

// Liveness
//
// Using the set of variables used after this instruction and the instruction
// itself, determines the set of variables which are used in or after this instruction.
fn ul_instr<'a>(instr: &'a X86Instr, live_after: HashSet<&'a str>) -> HashSet<&'a str> {
    let mut res = live_after.clone();
    for var in writes_of(instr) {
        let _ = res.remove(var);
    }
    for var in reads_of(instr) {
        let _ = res.insert(var);
    }
    res
}

fn ul_block(block: &[X86Instr]) -> Vec<HashSet<&str>> {
    let mut live_after_sets: Vec<HashSet<&str>> = vec![];
    let mut prev = HashSet::new();
    for instr in block.iter().rev() {
        live_after_sets.push(prev.clone());
        prev = ul_instr(instr, prev);
    }
    live_after_sets.reverse();
    live_after_sets
}

// Interference
type InterferenceGraph<'a> = petgraph::Graph<&'a str, ()>;

// Any variable that this instruction writes to, if it's read later in the program,
// represents an interference. These two variables cannot share a storage space.
fn build_interference_graph_instr<'a>(
    graph: &mut InterferenceGraph<'a>,
    instr: &'a X86Instr,
    live_after: &HashSet<&'a str>,
) {
    for write in writes_of(instr) {
        let write_idx = graph.add_node(write);
        for live in live_after {
            let live_idx = graph.add_node(live);
            graph.add_edge(write_idx, live_idx, ());
        }
    }
}

fn build_interference_graph_block<'a>(
    graph: &mut InterferenceGraph<'a>,
    block: &'a [X86Instr],
    live_afters: &[HashSet<&'a str>],
) {
    for (instr, live_after) in block.iter().zip(live_afters).rev() {
        build_interference_graph_instr(graph, instr, live_after);
    }
}

type Color = i32;

// X86 Helpers

fn as_var(arg: &X86Arg) -> Vec<&str> {
    match arg {
        X86Arg::Var(name) => vec![name],
        _ => vec![],
    }
}

fn reads_of(instr: &X86Instr) -> Vec<&str> {
    use X86Instr::*;
    match instr {
        Addq { val, rd } => as_var(val).into_iter().chain(as_var(rd)).collect(),
        Subq { val, rd } => as_var(val),
        Cmpq { a, b } => {
            let mut r = as_var(a);
            r.extend(as_var(b));
            r
        }
        Movq { src, rd } => as_var(src),
        Retq => vec![],
        Callq { label } => vec![],
        Je { .. } => vec![],
        Sete(rd) => vec![],
        Jmp{..} => vec![],
        _ => todo!(),
    }
}

fn writes_of(instr: &X86Instr) -> Vec<&str> {
    use X86Instr::*;
    match instr {
        Addq { val, rd } => as_var(rd),
        Subq { val, rd } => as_var(rd),
        Cmpq { .. } => vec![],
        Movq { src, rd } => as_var(rd),
        Retq => vec![],
        Callq { label } => vec![],
        Je { .. } => vec![],
        Sete(rd) => as_var(rd),
        Jmp{..} => vec![],
        x => todo!("{:?}", x),
    }
}
