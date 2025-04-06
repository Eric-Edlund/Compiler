use super::common::{X86Arg, X86Instr, X86Program};
use core::fmt;
use std::collections::{HashMap, HashSet};

const AVAILABLE_REGISTERS: &[&str] = &[
    "rdx", "rcx", "rsi", "r8", "r9", "r10", "rbx", "r12", "r13", "r14",
];
// Does not include rdi because we use that for function calls

#[derive(Default, Clone, Debug)]
struct ProgramPointMetadata {
    // The set of x86 variables which are live at this point.
    // These variables may be read before their values are intentionally discarded
    // by the program, so we must not overwrite them at this point.
    live: HashSet<String>,
}

#[derive(Debug)]
struct PartialSolution {
    // Each block of n instructions (which we assume to be executed linearly)
    // has n + 1 program points (0..n+1).
    program_pts: HashMap<String, Vec<ProgramPointMetadata>>,
}

/// The program is currently in pseudo x86 because it has variables.
/// In this pass we remove variables, replacing them with either
/// registers or stack references.
pub fn allocate_registers(program: &mut X86Program) {
    let mut solution = PartialSolution {
        program_pts: HashMap::new(),
    };
    for (label, instrs) in &program.blocks {
        let mut pts = Vec::<ProgramPointMetadata>::with_capacity(instrs.len() + 1);
        pts.extend(instrs.iter().map(|_| ProgramPointMetadata::default()));
        solution.program_pts.insert(label.to_string(), pts);
    }

    let mut changed = true;
    while changed {
        changed = false;
        dbg!("Live after pass");
        live_set_pass(program, &mut solution, &mut changed);
    }

    let mut interference_graph = InterferenceGraph::new();
    for (label, block) in &program.blocks {
        let live_sets: Vec<HashSet<String>> = solution.program_pts[label]
            .iter()
            .map(|pt| pt.live.clone())
            .collect::<Vec<_>>();
        extend_interference_graph(&mut interference_graph, &live_sets);
    }
    dbg!("Graph:", &interference_graph);
    let coloring = disjoint_coloring(&interference_graph);
    dbg!("Colorings:", &coloring);
    assert_eq!(coloring.len(), interference_graph.num_nodes());
    let variable_homes = assign_homes(&coloring);
    assert_eq!(coloring.len(), variable_homes.len());

    for (label, block) in &mut program.blocks {
        for instr in block {
            instr.transform_args(|arg| {
                use X86Arg::*;
                if let Var(name) = arg {
                    dbg!(&name);
                    *arg = variable_homes[name].clone();
                }
            })
        }
    }
}

// Improve the partial solution's approximation of the live after sets.
fn live_set_pass<'b, 'a: 'b>(
    program: &'a X86Program,
    ctx: &mut PartialSolution,
    changed: &mut bool,
) {
    for (label, block) in &program.blocks {
        let mut program_pts = ctx.program_pts.get_mut(label).unwrap().clone();
        // program_pts.first_mut().unwrap().live = HashSet::from_iter(["hi".to_string()]);
        for (i, pt) in program_pts.iter_mut().skip(1).enumerate().rev() {
            let next_instr: &'a X86Instr = &block[i];
            let values_discarded: HashSet<String> = writes_of(next_instr)
                .into_iter()
                .map(String::from)
                .collect();
            let values_read: HashSet<String> = reads_of(next_instr, ctx)
                .into_iter()
                .map(String::from)
                .collect();

            let prev = pt.live.clone();
            pt.live = pt.live.difference(&values_discarded).cloned().collect();
            pt.live = pt.live.union(&values_read).cloned().collect();
            if pt.live != prev {
                *changed = true;
            }
        }
        ctx.program_pts.insert(label.clone(), program_pts);
    }
}

#[derive(Debug)]
struct InterferenceGraph {
    adj_list: HashMap<String, HashSet<String>>,
}

impl InterferenceGraph {
    pub fn new() -> Self {
        InterferenceGraph {
            adj_list: HashMap::default(),
        }
    }

    pub fn num_nodes(&self) -> usize {
        self.adj_list.len()
    }

    pub fn add_node(&mut self, n: String) {
        if !self.adj_list.contains_key(&n) {
            self.adj_list.insert(n, HashSet::new());
        }
    }

    pub fn add<T>(&mut self, lhs: T, rhs: T)
    where
        T: Into<String> + Eq,
    {
        if rhs == lhs {
            return;
        }
        let lhs: String = lhs.into();
        let rhs: String = rhs.into();
        if let Some(l) = self.adj_list.get_mut(&lhs) {
            l.insert(rhs.clone());
        } else {
            self.adj_list
                .insert(lhs.clone(), HashSet::from_iter([rhs.clone()]));
        }
        if let Some(l) = self.adj_list.get_mut(&rhs) {
            l.insert(lhs.clone());
        } else {
            self.adj_list.insert(rhs, HashSet::from_iter([lhs]));
        }
    }

    pub fn nodes(&self) -> Vec<&String> {
        return self.adj_list.keys().collect();
    }

    pub fn neighbors(&self, n: &String) -> HashSet<&String> {
        return self.adj_list.get(n).unwrap().iter().collect();
    }
}

/// Any variables which are live at the same time at any point interfere.
fn extend_interference_graph<T>(graph: &mut InterferenceGraph, live_sets: &[HashSet<T>])
where
    T: AsRef<str> + fmt::Debug,
{
    for set in live_sets {
        for var in set {
            graph.add_node(var.as_ref().to_string());
        }
        if set.len() < 2 {
            continue;
        }
        let vars: Vec<&str> = set.iter().map(|s| s.as_ref()).collect();
        let mut first = 0;
        let last = set.len() - 1;
        while first < last {
            let a = vars[first];
            for i in first + 1..=last {
                let b = vars[i];
                graph.add(a, b);
            }
            first += 1;
        }
    }
}

type Color = i32;
fn disjoint_coloring<'a>(graph: &'a InterferenceGraph) -> HashMap<&'a str, Color> {
    let mut result = HashMap::<&str, Color>::new();
    // Each node's neighboring colors
    let mut saturation_sets = HashMap::<&str, HashSet<Color>>::new();

    let mut variables = HashSet::<&str>::new();
    for name in graph.nodes() {
        variables.insert(name);
        saturation_sets.insert(name, HashSet::new());
    }

    loop {
        let Some(&next) = variables
            .iter()
            .min_by_key(|name| saturation_sets[*name].len())
        else {
            break;
        };
        let name = next;
        variables.remove(&next);

        let color = (0..).find(|c| !saturation_sets[name].contains(c)).unwrap();
        result.insert(name, color);

        for neighbor in graph.neighbors(&name.to_string()) {
            saturation_sets
                .get_mut(neighbor.as_str())
                .unwrap()
                .insert(color);
        }
    }

    result
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

// X86 Helpers

fn as_var(arg: &X86Arg) -> Vec<&str> {
    match arg {
        X86Arg::Var(name) => vec![name],
        _ => vec![],
    }
}

fn reads_of<'b, 'a: 'b>(instr: &'a X86Instr, ctx: &'b PartialSolution) -> Vec<&'b str> {
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
        Je(label) | Jmp(label) => ctx.program_pts[label.as_str()]
            .first()
            .unwrap()
            .live
            .iter()
            .map(|s| s.as_str())
            .collect(),
        Sete(rd) => vec![],
        Setl(rd) => vec![],
        Setle(rd) => vec![],
        Setg(rd) => vec![],
        Setge(rd) => vec![],
        Setne(rd) => vec![],
        Andq(a, rd) => {
            let mut r = as_var(a);
            r.extend(as_var(rd));
            r
        }
        Orq(a, rd) => {
            let mut r = as_var(a);
            r.extend(as_var(rd));
            r
        }
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
        Setl(rd) => as_var(rd),
        Setle(rd) => as_var(rd),
        Setg(rd) => as_var(rd),
        Setge(rd) => as_var(rd),
        Setne(rd) => as_var(rd),
        Andq(a, rd) => as_var(rd),
        Orq(a, rd) => as_var(rd),
        Jmp { .. } => vec![],
        x => todo!("{:?}", x),
    }
}
