use super::x86::{X86Arg, X86Function, X86Instr, X86Program, CALLEE_SAVED_REGISTERS};
use core::fmt;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
};

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
pub fn allocate_registers(
    program: &mut X86Program,
    no_registers: bool,
    tuple_vars: &HashSet<String>,
) {
    for (name, function) in &mut program.functions {
        allocate_registers_function(function, no_registers, tuple_vars)
    }
}

fn allocate_registers_function(
    function: &mut X86Function,
    no_registers: bool,
    tuple_vars: &HashSet<String>,
) {
    let mut solution = PartialSolution {
        program_pts: HashMap::new(),
    };
    for (label, instrs) in &function.blocks {
        let mut pts = Vec::<ProgramPointMetadata>::with_capacity(instrs.len() + 1);
        pts.extend(
            instrs
                .iter()
                .map(|_| ProgramPointMetadata::default())
                .chain([ProgramPointMetadata::default()]),
        );
        assert!(pts.len() == instrs.len() + 1);
        solution.program_pts.insert(label.to_string(), pts);
    }

    let mut changed = true;
    while changed {
        changed = false;
        live_set_pass(function, &mut solution, &mut changed);
    }

    let mut interference_graph = InterferenceGraph::new();
    for (label, block) in &function.blocks {
        let live_sets: Vec<HashSet<String>> = solution.program_pts[label]
            .iter()
            .map(|pt| pt.live.clone())
            .collect::<Vec<_>>();
        extend_interference_graph(&mut interference_graph, &live_sets);
    }
    // complete_interference_graph(&mut interference_graph);

    let coloring = disjoint_coloring(&interference_graph);
    assert_eq!(coloring.len(), interference_graph.num_nodes());
    let (variable_homes, tuple_homes) = assign_homes(&coloring, no_registers, tuple_vars);
    assert_eq!(coloring.len(), variable_homes.len() + tuple_homes.len());

    for (label, block) in &mut function.blocks {
        *block = block
            .clone()
            .into_iter()
            .filter_map(|instr| {
                let try_discard = RefCell::new(false);
                let mut instr = instr.clone();
                instr.transform_args(|arg| {
                    use X86Arg::*;
                    if let Var(name) = arg {
                        if tuple_homes.contains_key(name) {
                            *arg = tuple_homes
                                .get(name)
                                // If a variable was not assigned a home, it must have never
                                // been live and it's value is a discardable byproduct.
                                .unwrap_or_else(|| {
                                    *try_discard.borrow_mut() = true;
                                    arg
                                })
                                .clone();
                        } else {
                            *arg = variable_homes
                                .get(name)
                                // If a variable was not assigned a home, it must have never
                                // been live and it's value is a discardable byproduct.
                                .unwrap_or_else(|| {
                                    *try_discard.borrow_mut() = true;
                                    arg
                                })
                                .clone();
                        }
                    }
                });
                if *try_discard.borrow() {
                    // Hopefully, the instruction's only write is to
                    // that variable and so we can drop it entirely.
                    let writes = writes_of(&instr);
                    for var in writes {
                        assert!(!variable_homes.contains_key(var));
                    }
                    None
                } else {
                    Some(instr)
                }
            })
            .collect();
    }
    function.stack_size = variable_homes
        .values()
        .filter(|home| matches!(home, X86Arg::Deref(_, _)))
        .count() as u32;
    function.stack_size *= 8;
    if function.stack_size % 16 != 0 {
        function.stack_size += 8;
    }
    function.stack_size += 16; // TODO: Why is this necessary?
    
    function.root_stack_size = tuple_homes
        .values()
        .count() as u32 * 8;
    if function.root_stack_size % 16 != 0 {
        function.root_stack_size += 8;
    }
}

// Improve the partial solution's approximation of the live after sets.
fn live_set_pass<'b, 'a: 'b>(
    function: &'a X86Function,
    ctx: &mut PartialSolution,
    changed: &mut bool,
) {
    for (label, block) in &function.blocks {
        let mut program_pts = ctx.program_pts.get_mut(label).unwrap().clone();
        // program_pts.first_mut().unwrap().live = HashSet::from_iter();
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
        self.adj_list.entry(n).or_default();
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

fn complete_interference_graph(graph: &mut InterferenceGraph)
{
    let nodes: Vec<String> = graph.nodes().iter().map(|r| r.to_string()).collect();
    for node in &nodes {
        for node1 in &nodes {
            if node == node1 {
                continue
            }
            graph.add(node, node1)
        }
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
            for b in &vars[first + 1..=last] {
                graph.add(a, b);
            }
            first += 1;
        }
    }
}

type Color = i32;
fn disjoint_coloring(graph: &InterferenceGraph) -> HashMap<&str, Color> {
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

/// Takes a set of variable names assigned to colors and creates an assignment from
/// variable names to either registers or derefs.
///
/// Returns (var homes, tuple var homes)
fn assign_homes(
    coloring: &HashMap<&str, Color>,
    no_registers: bool,
    tuple_var_names: &HashSet<String>,
) -> (HashMap<String, X86Arg>, HashMap<String, X86Arg>) {
    let mut vars: Vec<(&str, Color)> = vec![];
    let mut tuple_vars: Vec<(&str, Color)> = vec![];
    for (name, color) in coloring {
        if tuple_var_names.contains(&name.to_string()) {
            tuple_vars.push((*name, *color));
        } else {
            vars.push((*name, *color))
        }
    }
    vars.sort_by_key(|x| x.1);
    tuple_vars.sort_by_key(|x| x.1);
    let vars: Vec<&str> = vars.into_iter().map(|x| x.0).collect();

    // Assign regular var homes
    let reg_homes_iter = CALLEE_SAVED_REGISTERS
        .iter()
        .skip_while(|_| no_registers)
        .map(|reg_name| X86Arg::Reg(reg_name));
    let stack_homes_iter = (0..).map(|offset| X86Arg::Deref("rbp", -8 * (offset + 1)));
    let available_homes = reg_homes_iter.chain(stack_homes_iter);

    let var_homes =
        HashMap::from_iter(vars.into_iter().map(|x| x.to_string()).zip(available_homes));

    // Assign tuples homes on the root stack
    let tuple_vars: Vec<&str> = tuple_vars.into_iter().map(|x| x.0).collect();
    let root_stack_positions = (0..).map(|offset| X86Arg::Deref("r15", -8 * (offset + 1)));

    let tuple_var_homes = HashMap::from_iter(
        tuple_vars
            .into_iter()
            .zip(root_stack_positions)
            .map(|(name, arg)| (name.to_string(), arg)),
    );

    (var_homes, tuple_var_homes)
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
        Comment(..) => vec![],
        Addq(val, rd) => as_var(val).into_iter().chain(as_var(rd)).collect(),
        Subq(val, rd) => as_var(val).into_iter().chain(as_var(rd)).collect(),
        Imulq(val, rd) => as_var(val).into_iter().chain(as_var(rd)).collect(),
        Pushq(reg) => as_var(reg),
        Popq(deref) => vec![],
        Cmpq(a, b) => {
            let mut r = as_var(a);
            r.extend(as_var(b));
            r
        }
        Movq(src, rd) => as_var(src),
        Movzbq(src, rd) => as_var(src),
        Notq(rd) => as_var(rd),
        Xorq(a, rd) => as_var(rd).into_iter().chain(as_var(a)).collect(),
        Retq => vec![],
        Callq(label) => vec![],
        Je(label) | Jne(label) | Jmp(label) => {
            let live_before = ctx.program_pts[label.as_str()].first();
            live_before
                .unwrap()
                .live
                .iter()
                .map(|s| s.as_str())
                .collect()
        }
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
    }
}

fn writes_of(instr: &X86Instr) -> Vec<&str> {
    use X86Instr::*;
    match instr {
        Comment(..) => vec![],
        Pushq(reg) => vec![],
        Popq(deref) => as_var(deref),
        Addq(val, rd) => as_var(rd),
        Subq(val, rd) => as_var(rd),
        Imulq(a, rd) => as_var(rd),
        Cmpq { .. } => vec![],
        Movq(src, rd) => as_var(rd),
        Movzbq(src, rd) => as_var(rd),
        Notq(rd) => as_var(rd),
        Xorq(a, rd) => as_var(rd),
        Retq => vec![],
        Callq(label) => vec![],
        Je { .. } => vec![],
        Jne { .. } => vec![],
        Sete(rd) => as_var(rd),
        Setl(rd) => as_var(rd),
        Setle(rd) => as_var(rd),
        Setg(rd) => as_var(rd),
        Setge(rd) => as_var(rd),
        Setne(rd) => as_var(rd),
        Andq(a, rd) => as_var(rd),
        Orq(a, rd) => as_var(rd),
        Jmp { .. } => vec![],
    }
}
