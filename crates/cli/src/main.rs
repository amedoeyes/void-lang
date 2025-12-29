use clap::{Arg, Command, crate_name, crate_version, value_parser};
use core::fmt::{self, Display, Formatter};
use fxhash::FxHashSet;
use rustyline::DefaultEditor;
use std::{env, fs, io, path::PathBuf};
use void::{
    context::{Context, Node},
    error,
    eval::{self, evaluate},
    lexer::{self, Lexer, Token},
    modules,
    parser::{self, parse},
    type_system::{self, infer},
};

fn main() {
    if let Err(error) = run() {
        match error {
            Error::Clap(error) => {
                error.print().expect("error writing error");
                match error.kind() {
                    clap::error::ErrorKind::DisplayHelp
                    | clap::error::ErrorKind::DisplayVersion
                    | clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
                        std::process::exit(0)
                    }
                    _ => std::process::exit(1),
                }
            }
            _ => {
                eprintln!("{error}");
                std::process::exit(1);
            }
        }
    }
}

fn run() -> Result<()> {
    let cmd = Command::new(crate_name!())
        .version(crate_version!())
        .disable_colored_help(true)
        .disable_help_subcommand(true)
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(
            Command::new("tokens").arg(
                Arg::new("file")
                    .required(true)
                    .value_parser(value_parser!(PathBuf))
                    .help("source file"),
            ),
        )
        .subcommand(
            Command::new("nodes").arg(
                Arg::new("file")
                    .required(true)
                    .value_parser(value_parser!(PathBuf))
                    .help("source file"),
            ),
        )
        .subcommand(
            Command::new("type").arg(
                Arg::new("file")
                    .required(true)
                    .value_parser(value_parser!(PathBuf))
                    .help("source file"),
            ),
        )
        .subcommand(
            Command::new("eval")
                .arg(
                    Arg::new("file")
                        .required(true)
                        .value_parser(value_parser!(PathBuf))
                        .help("source file"),
                )
                .arg(Arg::new("args").num_args(0..)),
        )
        .subcommand(
            Command::new("repl").arg(
                Arg::new("file")
                    .value_parser(value_parser!(PathBuf))
                    .help("source file"),
            ),
        );

    match cmd.try_get_matches()?.subcommand() {
        Some(("tokens", sub_matches)) => tokens_cmd(sub_matches.get_one("file").unwrap()),
        Some(("nodes", sub_matches)) => nodes_cmd(sub_matches.get_one("file").unwrap()),
        Some(("type", sub_matches)) => type_cmd(sub_matches.get_one("file").unwrap()),
        Some(("eval", sub_matches)) => eval_cmd(sub_matches.get_one("file").unwrap()),
        Some(("repl", sub_matches)) => repl_cmd(sub_matches.get_one("file")),
        _ => unreachable!(),
    }
}

#[derive(Debug)]
enum Error {
    Clap(clap::Error),
    Void(error::Error),
    Io(io::Error),
}

impl From<std::io::Error> for Error {
    fn from(value: std::io::Error) -> Self {
        Error::Io(value)
    }
}

impl From<clap::Error> for Error {
    fn from(value: clap::Error) -> Self {
        Error::Clap(value)
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::Clap(err) => err.fmt(f),
            Error::Io(err) => err.fmt(f),
            Error::Void(err) => err.fmt(f),
        }
    }
}

type Result<T> = std::result::Result<T, Error>;

fn tokens_cmd(source_path: &PathBuf) -> Result<()> {
    let contents = fs::read_to_string(source_path)?;
    let mut lexer = Lexer::new(&contents);

    loop {
        match lexer.next_token() {
            Ok((token, span)) => {
                if token == Token::Eof {
                    break;
                }
                println!(
                    "{}:{}:{}: {:?}",
                    source_path.display(),
                    span.start.line,
                    span.end.column,
                    token
                );
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

fn nodes_cmd(source_path: &PathBuf) -> Result<()> {
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
            Node::Expr(_) => {
                println!(
                    "{} : {}",
                    node.display(&ctx),
                    ctx.get_type(node).as_ref().unwrap()
                )
            }
            Node::Bind(name, _) => println!("{} : {}", name, ctx.get_type(node).as_ref().unwrap()),
            Node::Import(_) => continue,
        }
    }

    Ok(())
}

fn eval_cmd(source_path: &PathBuf) -> Result<()> {
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

    let value = match evaluate(&ctx, &nodes) {
        Ok(value) => value,
        Err(err) => {
            return Err(Error::Void(error::Error::Eval(
                source_path.clone(),
                contents,
                err,
            )));
        }
    };

    println!("{}", value.display(&ctx));

    Ok(())
}

fn repl_cmd(source_path: Option<&PathBuf>) -> Result<()> {
    let mut ctx = Context::new();

    let mut rl = DefaultEditor::new().expect("could not initialize line editor");

    let mut nodes = Vec::new();

    if let Some(source_path) = source_path {
        let parent_dir = source_path
            .parent()
            .ok_or_else(|| Error::Void(error::Error::InvalidPath(source_path.clone())))?;

        let mut visited_modules = FxHashSet::default();
        visited_modules.insert(PathBuf::from(source_path));

        let contents = fs::read_to_string(source_path)?;
        nodes.extend(match parse(&mut ctx, &contents) {
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
        });

        if let Err(err) = infer(&mut ctx, &nodes) {
            return Err(Error::Void(error::Error::Type(
                source_path.clone(),
                contents,
                Box::new(err),
            )));
        }
    }

    let cwd = env::current_dir()?;
    let parent_dir = cwd
        .parent()
        .ok_or_else(|| Error::Void(error::Error::InvalidPath(cwd.clone())))?;

    loop {
        let input = match rl.readline("> ") {
            Ok(line) => {
                rl.add_history_entry(&line)
                    .expect("could not add history entry");
                line
            }
            Err(rustyline::error::ReadlineError::Interrupted) => {
                continue;
            }
            Err(_) => {
                break;
            }
        };

        if input.trim().is_empty() {
            continue;
        }

        nodes.extend(match parse(&mut ctx, &input) {
            Ok(nodes) => modules::resolve_imports(
                &mut ctx,
                &nodes,
                parent_dir,
                &mut FxHashSet::default(),
                &mut FxHashSet::default(),
            )
            .map_err(Error::Void)?,

            Err(err) => {
                match err {
                    parser::Error::Lexer(lexer::Error::InvalidToken(span))
                    | parser::Error::Lexer(lexer::Error::Unterminated(_, span))
                    | parser::Error::Lexer(lexer::Error::EmptyChar(span))
                    | parser::Error::Lexer(lexer::Error::InvalidChar(span))
                    | parser::Error::Lexer(lexer::Error::InvalidEscapeChar(span))
                    | parser::Error::UnexpectedToken(_, (_, span)) => {
                        println!(
                            "{}:{}: {}",
                            span.start.line,
                            span.start.column,
                            &err.to_string()
                        );
                    }
                }
                continue;
            }
        });

        if let Err(err) = infer(&mut ctx, &nodes) {
            match err {
                type_system::Error::TypeMismatch(ty1, ty2, span) => {
                    println!(
                        "{}:{}: expected type '{ty1}', but found '{ty2}'",
                        span.start.line, span.start.column
                    );
                }
                type_system::Error::InfiniteType(ty, span) => {
                    println!(
                        "{}:{}: infinite type '{ty}'",
                        span.start.line, span.start.column
                    );
                }
                type_system::Error::UnknownIdentifier(id, span) => {
                    println!(
                        "{}:{}: unknown identifier '{id}'",
                        span.start.line, span.start.column
                    );
                }
                type_system::Error::UnknownOperator(op, span) => {
                    println!(
                        "{}:{}: unknown operator '({op})'",
                        span.start.line, span.start.column
                    );
                }
                type_system::Error::NoInstance(cons, ty, span) => {
                    println!(
                        "{}:{}: No '{cons}' instance for type '{ty}'",
                        span.start.line, span.start.column
                    );
                }
            }
            nodes.pop();
            continue;
        }

        if let Node::Expr(_) = ctx.get_node(*nodes.last().unwrap()) {
            let value = match evaluate(&ctx, &nodes) {
                Ok(value) => value,
                Err(err) => {
                    match err {
                        eval::Error::DivisionByZero(span) => println!(
                            "{}:{}: division by zero",
                            span.start.line, span.start.column
                        ),
                        eval::Error::EmptyList(span) => {
                            println!("{}:{}: list is empty", span.start.line, span.start.column)
                        }
                        eval::Error::IO(message, span) => {
                            println!("{}:{}: {message}", span.start.line, span.start.column)
                        }
                    }
                    continue;
                }
            };

            println!("{}", value.display(&ctx));
            nodes.pop();
        }
    }

    Ok(())
}
