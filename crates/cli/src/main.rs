use core::fmt::{self, Display, Formatter};
use std::{
    env,
    fs::{self, File},
    io::{self, BufWriter},
    path::PathBuf,
    process::{self, Command},
};

use clap::{Parser, Subcommand, ValueEnum, crate_name, crate_version};
use void::{
    ast::{arena::NodeArena, node::NodeKind},
    codegen::{self},
    error,
    interperter::GMachine,
    ir::{Instruction, generate},
    lexer::{Lexer, Token},
    parser::{self, parse},
    type_system,
};

const PRELUDE: &str = include_str!("../../lib/prelude.void");

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
    Lex {
        file: PathBuf,
    },
    Parse {
        file: PathBuf,
    },
    Type {
        file: PathBuf,
    },
    Ir {
        file: PathBuf,
    },
    Compile {
        file: PathBuf,

        #[arg(short, long, value_enum, default_value_t = Emit::Binary)]
        emit: Emit,

        #[arg(short, long)]
        output: Option<PathBuf>,

        #[arg(short, long)]
        debug: bool,

        #[arg(short, long)]
        run: bool,
    },
    Run {
        file: PathBuf,
    },
}

#[derive(ValueEnum, Clone, Copy)]
enum Emit {
    #[clap(alias = "obj")]
    Object,
    #[clap(alias = "asm")]
    Assembly,
    #[clap(alias = "bin")]
    Binary,
}

fn main() {
    let res = match Cli::parse().command {
        Commands::Lex { file } => lex_cmd(&file),
        Commands::Parse { file } => parse_cmd(&file),
        Commands::Type { file } => type_cmd(&file),
        Commands::Ir { file } => ir_cmd(&file),
        Commands::Compile {
            file,
            emit,
            output,
            debug,
            run,
        } => compile_cmd(&file, emit, output, debug, run),
        Commands::Run { file } => run_cmd(&file),
    };

    if let Err(e) = res {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn ir_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    type_system::infer(&mut ctx);

    let ir = generate(&ctx);

    for (name, insts) in ir {
        println!("{}:", name);
        for i in insts {
            println!("  {i}");
        }
        println!();
    }

    Ok(())
}

fn compile_cmd(
    source_path: &PathBuf,
    emit: Emit,
    output: Option<PathBuf>,
    debug: bool,
    run: bool,
) -> Result<()> {
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    type_system::infer(&mut ctx);

    let symbols = generate(&ctx);

    let asm_path = source_path
        .clone()
        .file_stem()
        .map(|p| PathBuf::from(p).with_extension("nasm"))
        .unwrap();
    let obj_path = asm_path.with_extension("o");
    let asm_file = File::create(&asm_path)?;
    let mut asm_buf = BufWriter::new(&asm_file);

    codegen::x86_64::emit(&mut asm_buf, &symbols)?;

    if matches!(emit, Emit::Assembly) {
        return Ok(());
    }

    let mut cmd = Command::new("nasm");
    cmd.args(&["-f", "elf64"]);
    if debug {
        cmd.args(&["-g", "-F", "dwarf"]);
    }
    cmd.arg(&asm_path);

    let status = cmd.status().expect("Failed to execute nasm");

    fs::remove_file(&asm_path)?;

    if !status.success() {
        process::exit(1);
    }

    if matches!(emit, Emit::Object) {
        return Ok(());
    }

    let mut cmd = Command::new("mold");
    cmd.arg("-static");

    if let Some(output) = &output {
        cmd.arg("-o").arg(output);
    }
    cmd.arg(&obj_path);

    let status = cmd.status().expect("Failed to execute ld");

    fs::remove_file(&obj_path)?;

    if !status.success() {
        process::exit(1);
    }

    if run {
        let bin = output
            .map(|o| PathBuf::from("./").join(o))
            .unwrap_or_else(|| PathBuf::from("./a.out"));
        Command::new(bin)
            .status()
            .expect("Failed to execute binary");
    }

    Ok(())
}

fn run_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    type_system::infer(&mut ctx);

    let symbols = generate(&ctx);

    let start_instructions = &[
        &Instruction::PushGlobal("main".into(), 0),
        &Instruction::Eval,
    ];

    let mut machine = GMachine::new(&symbols, start_instructions);

    machine.run();

    if let Some(res) = machine.stack.last() {
        machine.println(*res)
    }

    Ok(())
}

fn lex_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

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
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

    let modules = ctx
        .kinds()
        .iter()
        .filter_map(|n| match n {
            NodeKind::Module(nodes) => Some(nodes.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for module in modules {
        for node in module {
            println!("{}", node.display(&ctx));
        }
    }

    Ok(())
}

fn type_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = NodeArena::new();

    parse(&mut ctx, &PRELUDE).unwrap();

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    type_system::infer(&mut ctx);

    let modules = ctx
        .kinds()
        .iter()
        .filter_map(|n| match n {
            NodeKind::Module(nodes) => Some(nodes.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for module in modules {
        for node in module {
            match ctx.kind(node) {
                NodeKind::Primitive(name, ..) | NodeKind::Bind(name, ..) => {
                    println!("{} : {}", name, ctx.ty(node).as_ref().unwrap())
                }
                _ => continue,
            }
        }
    }

    Ok(())
}
