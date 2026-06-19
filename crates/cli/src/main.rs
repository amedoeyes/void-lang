use core::fmt::{self, Display, Formatter};
use std::{
    env,
    fs::{self, File},
    io::{self, BufWriter},
    path::PathBuf,
    process::Command,
};

use clap::{Parser, Subcommand, crate_name, crate_version};
use fxhash::FxHashSet;
use void::{
    codegen::{self},
    context::{Context, Node},
    error,
    ir::{generate, instructions::Instruction, interperter::GMachine},
    lexer::{Lexer, Token},
    parser::{self, parse},
    type_system::infer,
};

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
    Ir { file: PathBuf },
    Compile { file: PathBuf },
    Run { file: PathBuf },
}

fn main() {
    let res = match Cli::parse().command {
        Commands::Lex { file } => lex_cmd(&file),
        Commands::Parse { file } => parse_cmd(&file),
        Commands::Type { file } => type_cmd(&file),
        // Commands::Repl { file } => repl_cmd(file.as_ref()),
        Commands::Ir { file } => ir_cmd(&file),
        Commands::Compile { file } => compile_cmd(&file),
        Commands::Run { file } => run_cmd(&file),
    };

    if let Err(e) = res {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}

fn ir_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    // let parent_dir = source_path
    //     .parent()
    //     .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    infer(&mut ctx).map_err(|err| {
        Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

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

fn compile_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    // let parent_dir = source_path
    //     .parent()
    //     .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    infer(&mut ctx).map_err(|err| {
        Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

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

    let status = Command::new("nasm")
        .args(&["-f", "elf64"])
        .arg(&asm_path)
        .status()
        .expect("Failed to execute nasm");

    if !status.success() {
        return Ok(());
    }

    let status = Command::new("ld")
        .arg(&obj_path)
        .status()
        .expect("Failed to execute ld");

    if !status.success() {
        return Ok(());
    }

    fs::remove_file(&asm_path)?;
    fs::remove_file(&obj_path)?;

    Ok(())
}

fn run_cmd(source_path: &PathBuf) -> Result<()> {
    let mut ctx = Context::new();

    // let parent_dir = source_path
    //     .parent()
    //     .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    infer(&mut ctx).map_err(|err| {
        Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

    let symbols = generate(&ctx);

    let mut machine = GMachine::new(&symbols);
    machine.instructions =
        Vec::from([Instruction::PushGlobal("main".into(), 0), Instruction::Eval]);

    machine.run();

    if let Some(res) = machine.stack.last() {
        machine.println(*res)
    }

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

    // let parent_dir = source_path
    //     .parent()
    //     .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

    let modules = ctx
        .nodes()
        .iter()
        .filter_map(|n| match n {
            Node::Module(nodes) => Some(nodes.clone()),
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
    let mut ctx = Context::new();

    // let parent_dir = source_path
    //     .parent()
    //     .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

    let mut visited_modules = FxHashSet::default();
    visited_modules.insert(PathBuf::from(source_path));

    let contents = fs::read_to_string(source_path)?;

    parse(&mut ctx, &contents).map_err(|err| {
        Error::Void(error::Error::Syntax(
            source_path.clone(),
            contents.clone(),
            Box::new(err),
        ))
    })?;

    infer(&mut ctx).map_err(|err| {
        Error::Void(error::Error::Type(
            source_path.clone(),
            contents,
            Box::new(err),
        ))
    })?;

    let modules = ctx
        .nodes()
        .iter()
        .filter_map(|n| match n {
            Node::Module(nodes) => Some(nodes.clone()),
            _ => None,
        })
        .collect::<Vec<_>>();

    for module in modules {
        for node in module {
            match ctx.get_node(node) {
                Node::TypeExpr(..) => todo!(),
                Node::Expr(..) => {
                    println!(
                        "{} : {}",
                        node.display(&ctx),
                        ctx.get_type(node).as_ref().unwrap()
                    )
                }
                Node::Bind(name, _) => {
                    println!("{} : {}", name, ctx.get_type(node).as_ref().unwrap())
                }
                _ => continue,
            }
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
