use clap::{Parser, Subcommand, crate_name, crate_version};
use core::fmt::{self, Display, Formatter};
use fxhash::FxHashSet;
use std::{collections::HashMap, env, fs, io, path::PathBuf};
use void::{
    context::{Context, Node, NodeId},
    error,
    expr::Expr,
    lexer::{Lexer, Token},
    modules,
    parser::{self, parse},
    type_system::infer,
};

#[derive(Debug, Clone)]
struct Super {
    name: String,
    arity: usize,
    instructions: Vec<Instruction>,
}

impl Super {
    fn new(name: String) -> Self {
        Self {
            name,
            arity: 0,
            instructions: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum Value {
    Unit,
    Integer(i64),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Unit => writeln!(f, "()"),
            Value::Integer(i) => writeln!(f, "{i}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Address(usize);

#[derive(Debug, Clone)]
enum Global {
    Builtin,
    Super(Vec<Instruction>),
}

#[derive(Debug, Clone)]
enum GNode {
    Integer(i64),
    Application(Address, Address),
    Global(String, usize, Global),
    Indirection(Address),
}

#[derive(Debug, Clone)]
enum Instruction {
    PushInt(i64),
    Push(usize),
    PushGlobal(String),
    Pop(usize),
    Update(usize),
    MkAp,
    Cond(Vec<Instruction>, Vec<Instruction>),
    Eval,
    Unwind,
}

#[derive(Debug)]
struct GMachine {
    pc: usize,
    instructions: Vec<Instruction>,
    globals: HashMap<String, Address>,
    heap: Vec<GNode>,
    stack: Vec<Address>,
    dump: Vec<(Vec<Address>, Vec<Instruction>)>,
}

impl GMachine {
    fn new() -> Self {
        Self {
            pc: 0,
            instructions: Vec::new(),
            globals: HashMap::new(),
            heap: Vec::new(),
            stack: Vec::new(),
            dump: Vec::new(),
        }
    }

    fn alloc(&mut self, node: GNode) -> Address {
        let addr = self.heap.len();
        self.heap.push(node);
        Address(addr)
    }

    fn is_whnf(&self, addr: Address) -> bool {
        match self.heap[addr.0] {
            GNode::Integer(_) => true,
            GNode::Application(_, _) => false,
            GNode::Global(_, _, _) => false,
            GNode::Indirection(n) => self.is_whnf(n),
        }
    }

    fn run(&mut self) -> Value {
        loop {
            if self.pc >= self.instructions.len() {
                break;
            }

            let inst = self.instructions[self.pc].clone();

            println!("------");
            println!("pc: {}", self.pc);
            println!("instructions: {:?}", self.instructions);
            println!("stack: {:?}", self.stack);
            println!("heap: {:?}", self.heap);
            println!("dump: {:?}", self.dump);

            println!("inst: {:?}", inst);

            match inst {
                Instruction::PushInt(i) => {
                    let addr = self.alloc(GNode::Integer(i));
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::PushGlobal(name) => {
                    let addr = *self.globals.get(&name).unwrap();
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::Push(n) => {
                    let addr = self.stack[self.stack.len() - 1 - n];
                    self.stack.push(addr);
                    self.pc += 1;
                }
                Instruction::Pop(n) => {
                    self.stack.truncate(self.stack.len() - n);
                    self.pc += 1;
                }
                Instruction::Update(n) => {
                    let addr = self.stack.pop().unwrap();
                    let root = self.stack[self.stack.len() - 1 - n];
                    self.heap[root.0] = GNode::Indirection(addr);
                    self.pc += 1;
                }
                Instruction::MkAp => {
                    let l = self.stack.pop().unwrap();
                    let r = self.stack.pop().unwrap();
                    let app = self.alloc(GNode::Application(l, r));
                    self.stack.push(app);
                    self.pc += 1;
                }
                Instruction::Cond(t, f) => {
                    let addr = self.stack.pop().unwrap();
                    match self.heap[addr.0] {
                        GNode::Integer(i) if i == 1 => {
                            self.instructions.splice(self.pc + 1..self.pc + 1, t);
                        }
                        GNode::Integer(i) if i == 0 => {
                            self.instructions.splice(self.pc + 1..self.pc + 1, f);
                        }
                        _ => unreachable!(),
                    }
                    self.pc += 1;
                }
                Instruction::Eval => {
                    let top = *self.stack.last().unwrap();
                    if !self.is_whnf(top) {
                        self.dump.push((
                            self.stack[..self.stack.len().saturating_sub(1)].to_vec(),
                            self.instructions[self.pc + 1..].to_vec(),
                        ));
                        self.pc = 0;
                        self.instructions = Vec::from([Instruction::Unwind]);
                        self.stack = Vec::from([top]);
                    } else {
                        self.pc += 1;
                    }
                }
                Instruction::Unwind => {
                    let top = *self.stack.last().unwrap();

                    match self.heap[top.0].clone() {
                        GNode::Integer(i) => {
                            println!("Integer({i})");
                            if !self.dump.is_empty() {
                                let (stack, instructions) = self.dump.pop().unwrap();
                                self.stack = stack;
                                self.stack.push(top);
                                self.instructions = instructions;
                                self.pc = 0;
                            } else {
                                self.pc += 1;
                            }
                        }
                        GNode::Application(l, r) => {
                            println!("Application({l:?}, {r:?})");
                            self.stack.push(l);
                        }
                        GNode::Indirection(addr) => {
                            println!("Indirection({addr:?})");
                            *self.stack.last_mut().unwrap() = addr;
                        }
                        GNode::Global(name, arity, global) => {
                            println!("Global({name}, {arity}, {global:?})");
                            if self.stack.len() <= arity {
                                self.pc += 1;
                            } else {
                                let mut args = self.stack[..arity]
                                    .iter()
                                    .try_fold(Vec::with_capacity(arity), |mut acc, a| {
                                        if let GNode::Application(_, r) = self.heap[a.0] {
                                            acc.push(r);
                                            Ok(acc)
                                        } else {
                                            Err(String::from("expected application"))
                                        }
                                    })
                                    .unwrap();

                                println!("fun: {name}, args: {args:?}");

                                let redex_root = self.stack[self.stack.len() - 1 - arity];
                                match global {
                                    Global::Builtin => match name.as_str() {
                                        "+" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l + r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "-" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l - r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "*" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr = self.alloc(GNode::Integer(l * r));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "==" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l == r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        "<=" => {
                                            let lhs_addr = args.pop().unwrap();
                                            let rhs_addr = args.pop().unwrap();
                                            let lhs = &self.heap[lhs_addr.0];
                                            let rhs = &self.heap[rhs_addr.0];
                                            if let (GNode::Integer(l), GNode::Integer(r)) =
                                                (lhs, rhs)
                                            {
                                                let result_addr =
                                                    self.alloc(GNode::Integer((l <= r) as i64));
                                                self.stack.push(result_addr);
                                            }
                                        }
                                        _ => unreachable!(),
                                    },
                                    Global::Super(instructions) => {
                                        self.pc = 0;
                                        self.instructions = instructions;
                                        self.stack = Vec::with_capacity(arity + 1);
                                        self.stack.push(redex_root);
                                        self.stack.extend(args);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            println!("instructions: {:?}", self.instructions);
            println!("stack: {:?}", self.stack);
            println!("heap: {:?}", self.heap);
            println!("dump: {:?}", self.dump);
        }

        let result_addr = *self.stack.last().unwrap();
        println!("------");
        println!("result: {:?}", self.heap[result_addr.0]);
        match self.heap[result_addr.0] {
            GNode::Integer(i) => Value::Integer(i),
            _ => Value::Unit,
        }
    }
}

fn compile_expr(
    ctx: &Context,
    node: NodeId,
    params: &HashMap<String, usize>,
    out: &mut Vec<Instruction>,
) {
    match ctx.get_node(node) {
        Node::Expr(expr) => match expr {
            Expr::Boolean(b) => out.push(Instruction::PushInt(*b as i64)),
            Expr::Integer(i) => out.push(Instruction::PushInt(*i)),
            Expr::Identifier(id) => {
                if let Some(offset) = params.get(id) {
                    out.push(Instruction::Push(*offset));
                } else {
                    out.push(Instruction::PushGlobal(id.clone()));
                }
            }
            Expr::Application { func, arg } => {
                compile_expr(&ctx, *arg, params, out);
                compile_expr(
                    &ctx,
                    *func,
                    &params.iter().map(|(k, v)| (k.clone(), v + 1)).collect(),
                    out,
                );
                out.push(Instruction::MkAp);
            }
            Expr::Condition { cond, then, alt } => {
                compile_expr(&ctx, *cond, params, out);
                out.push(Instruction::Eval);
                let mut then_insts = Vec::new();
                compile_expr(&ctx, *then, &params, &mut then_insts);
                let mut alt_insts = Vec::new();
                compile_expr(&ctx, *alt, &params, &mut alt_insts);
                out.push(Instruction::Cond(then_insts, alt_insts));
            }
            other => todo!("{other:?}"),
        },
        _ => unreachable!(),
    }
}

fn compile(ctx: &Context, nodes: &[NodeId]) -> Vec<Super> {
    let mut ir = Vec::new();
    for node in nodes {
        match ctx.get_node(*node).clone() {
            Node::Bind(name, expr) => {
                let mut sc = Super::new(name);
                match ctx.get_node(expr).clone() {
                    Node::Expr(Expr::Lambda { .. }) => {
                        let mut params = HashMap::new();
                        let mut node = expr;
                        while let Node::Expr(Expr::Lambda { param, body }) = ctx.get_node(node) {
                            params.insert(param.clone(), params.len());
                            node = *body;
                        }
                        sc.arity = params.len();
                        compile_expr(ctx, node, &params, &mut sc.instructions);
                    }
                    _ => compile_expr(ctx, expr, &HashMap::new(), &mut sc.instructions),
                }
                sc.instructions.push(Instruction::Update(sc.arity));
                sc.instructions.push(Instruction::Pop(sc.arity));
                sc.instructions.push(Instruction::Unwind);
                ir.push(sc);
            }
            _ => continue,
        }
    }
    ir
}

#[derive(Debug)]
enum Error {
    Void(error::Error),
    Io(io::Error),
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Io(err) => err.fmt(f),
            Error::Void(err) => err.fmt(f),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Parser)]
#[command(name = crate_name!())]
#[command(version = crate_version!())]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Lex { file: PathBuf },
    Parse { file: PathBuf },
    Type { file: PathBuf },
    // Repl { file: Option<PathBuf> },
    Compile { file: PathBuf },
    Run { file: PathBuf },
}

fn main() {
    let res = match Cli::parse().command {
        Commands::Lex { file } => lex_cmd(&file),
        Commands::Parse { file } => parse_cmd(&file),
        Commands::Type { file } => type_cmd(&file),
        // Commands::Repl { file } => repl_cmd(file.as_ref()),
        Commands::Compile { file } => compile_cmd(&file),
        Commands::Run { file } => run_cmd(&file),
    };

    if let Err(e) = res {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn compile_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    let ir = compile(&ctx, &nodes);

    println!("{ir:#?}");

    Ok(())
}

fn run_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    let ir = compile(&ctx, &nodes);

    let mut machine = GMachine::new();
    machine.instructions = Vec::from([Instruction::PushGlobal("main".into()), Instruction::Eval]);

    for s in ir {
        let global = machine.alloc(GNode::Global(
            s.name.clone(),
            s.arity,
            Global::Super(s.instructions),
        ));
        machine.globals.insert(s.name.clone(), global);
    }

    let add_global = machine.alloc(GNode::Global("+".into(), 2, Global::Builtin));
    let sub_global = machine.alloc(GNode::Global("-".into(), 2, Global::Builtin));
    let mul_global = machine.alloc(GNode::Global("*".into(), 2, Global::Builtin));
    let eq_global = machine.alloc(GNode::Global("==".into(), 2, Global::Builtin));
    let le_global = machine.alloc(GNode::Global("<=".into(), 2, Global::Builtin));
    machine.globals.insert("+".into(), add_global);
    machine.globals.insert("-".into(), sub_global);
    machine.globals.insert("*".into(), mul_global);
    machine.globals.insert("==".into(), eq_global);
    machine.globals.insert("<=".into(), le_global);

    let res = machine.run();

    println!("{res}");

    Ok(())
}

fn lex_cmd(source_path: &PathBuf) -> Result<()> {
    let contents = fs::read_to_string(source_path)?;
    let mut lexer = Lexer::new(&contents);

    loop {
        match lexer.next_token() {
            Ok((token, span)) => {
                println!(
                    "{}:{}:{}: {:?}",
                    source_path.display(),
                    span.start.line,
                    span.end.column,
                    token
                );
                if token == Token::Eof {
                    break;
                }
            }
            Err(err) => {
                return Err(Error::Void(error::Error::Syntax(
                    source_path.clone(),
                    contents,
                    Box::new(parser::Error::Lexer(err)),
                )));
            }
        }
    }

    Ok(())
}

fn parse_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    for node in nodes {
        println!("{}", node.display(&ctx));
    }

    Ok(())
}

fn type_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    let parent_dir = source_path
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;
    let nodes = match parse(&mut ctx, &contents) {
        Ok(nodes) => modules::resolve_imports(
            &mut ctx,
            &nodes,
            parent_dir,
            &mut visited_modules,
            &mut FxHashSet::default(),
        )
        .map_err(Error::Void)?,
        Err(err) => {
            return Err(Error::Void(error::Error::Syntax(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    };

    if let Err(err) = infer(&mut ctx, &nodes) {
        return Err(Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        )));
    }

    for node in nodes {
        match ctx.get_node(node) {
            Node::TypeExpr(..) => todo!(),
            Node::Expr(..) => {
                println!(
                    "{} : {}",
                    node.display(&ctx),
                    ctx.get_type(node).as_ref().unwrap()
                )
            }
            Node::Type(..) => {
                println!(
                    "{} : {}",
                    node.display(&ctx),
                    ctx.get_type(node).as_ref().unwrap()
                )
            }
            Node::Bind(name, _) => println!("{} : {}", name, ctx.get_type(node).as_ref().unwrap()),
            Node::Import(..) => continue,
        }
    }

    Ok(())
}

// fn repl_cmd(source_path: Option<&PathBuf>) -> Result<()> {
//     let mut ctx = Context::new();

//     let mut rl = DefaultEditor::new().expect("could not initialize line editor");

//     let mut nodes = Vec::new();

//     if let Some(source_path) = source_path {
//         let parent_dir = source_path
//             .parent()
//             .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

//         let mut visited_modules = FxHashSet::default();
//         visited_modules.insert(PathBuf::from(source_path));

//         let contents = fs::read_to_string(source_path)?;
//         nodes.extend(match parse(&mut ctx, &contents) {
//             Ok(nodes) => modules::resolve_imports(
//                 &mut ctx,
//                 &nodes,
//                 parent_dir,
//                 &mut visited_modules,
//                 &mut FxHashSet::default(),
//             )
//             .map_err(Error::Void)?,
//             Err(err) => {
//                 return Err(Error::Void(error::Error::Syntax(
//                     source_path.clone(),
//                     contents,
//                     Box::new(err),
//                 )));
//             }
//         });

//         if let Err(err) = infer(&mut ctx, &nodes) {
//             return Err(Error::Void(error::Error::Type(
//                 source_path.clone(),
//                 contents,
//                 Box::new(err),
//             )));
//         }
//     }

//     let cwd = env::current_dir()?;
//     let parent_dir = cwd
//         .parent()
//         .ok_or_else(|| Error::Void(error::Error::InvalidPath(cwd.clone())))?;

//     loop {
//         let input = match rl.readline("> ") {
//             Ok(line) => {
//                 rl.add_history_entry(&line)
//                     .expect("could not add history entry");
//                 line
//             }
//             Err(rustyline::error::ReadlineError::Interrupted) => {
//                 continue;
//             }
//             Err(_) => {
//                 break;
//             }
//         };

//         if input.trim().is_empty() {
//             continue;
//         }

//         nodes.extend(match parse(&mut ctx, &input) {
//             Ok(nodes) => modules::resolve_imports(
//                 &mut ctx,
//                 &nodes,
//                 parent_dir,
//                 &mut FxHashSet::default(),
//                 &mut FxHashSet::default(),
//             )
//             .map_err(Error::Void)?,

//             Err(err) => {
//                 match err {
//                     parser::Error::Lexer(lexer::Error::InvalidToken(span))
//                     | parser::Error::Lexer(lexer::Error::Unterminated(span, _))
//                     | parser::Error::Lexer(lexer::Error::EmptyChar(span))
//                     | parser::Error::Lexer(lexer::Error::InvalidChar(span))
//                     | parser::Error::Lexer(lexer::Error::InvalidEscapeChar(span))
//                     | parser::Error::UnexpectedToken(_, (_, span)) => {
//                         println!(
//                             "{}:{}: {}",
//                             span.start.line,
//                             span.start.column,
//                             &err.to_string()
//                         );
//                     }
//                 }
//                 continue;
//             }
//         });

//         if let Err(err) = infer(&mut ctx, &nodes) {
//             match err {
//                 type_system::Error::TypeMismatch(ty1, ty2, span) => {
//                     println!(
//                         "{}:{}: expected type '{ty1}', but found '{ty2}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::InfiniteType(ty, span) => {
//                     println!(
//                         "{}:{}: infinite type '{ty}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::UnknownIdentifier(id, span) => {
//                     println!(
//                         "{}:{}: unknown identifier '{id}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::UnknownOperator(op, span) => {
//                     println!(
//                         "{}:{}: unknown operator '({op})'",
//                         span.start.line, span.start.column
//                     );
//                 }
//                 type_system::Error::NoInstance(cons, ty, span) => {
//                     println!(
//                         "{}:{}: No '{cons}' instance for type '{ty}'",
//                         span.start.line, span.start.column
//                     );
//                 }
//             }
//             nodes.pop();
//             continue;
//         }

//         if let Node::Expr(_) = ctx.get_node(*nodes.last().unwrap()) {
//             let value = match evaluate(&ctx, &nodes) {
//                 Ok(value) => value,
//                 Err(err) => {
//                     match err {
//                         eval::Error::DivisionByZero(span) => println!(
//                             "{}:{}: division by zero",
//                             span.start.line, span.start.column
//                         ),
//                         eval::Error::EmptyList(span) => {
//                             println!("{}:{}: list is empty", span.start.line, span.start.column)
//                         }
//                         eval::Error::IO(message, span) => {
//                             println!("{}:{}: {message}", span.start.line, span.start.column)
//                         }
//                     }
//                     continue;
//                 }
//             };

//             println!("{}", value.display(&ctx));
//             nodes.pop();
//         }
//     }

//     Ok(())
// }
