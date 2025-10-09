mod context;
mod error;
mod eval;
mod expr;
mod lexer;
mod parser;
mod span;
mod type_system;

use clap::{Arg, Command, crate_name, crate_version};
use rustyline::DefaultEditor;
use std::fs;

use crate::{
    context::{Context, Node},
    error::{Error, Result, SyntaxError},
    eval::evaluate,
    lexer::{Lexer, Token},
    parser::parse,
    type_system::infer,
};

fn run() -> Result<()> {
    let cmd = Command::new(crate_name!())
        .version(crate_version!())
        .disable_colored_help(true)
        .disable_help_subcommand(true)
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(Command::new("tokens").arg(Arg::new("file").required(true).help("source file")))
        .subcommand(Command::new("nodes").arg(Arg::new("file").required(true).help("source file")))
        .subcommand(Command::new("type").arg(Arg::new("file").required(true).help("source file")))
        .subcommand(
            Command::new("eval")
                .arg(Arg::new("file").required(true).help("source file"))
                .arg(Arg::new("args").num_args(0..)),
        )
        .subcommand(Command::new("repl").arg(Arg::new("file").help("source file")));

    match cmd.try_get_matches()?.subcommand() {
        Some(("tokens", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut lexer = Lexer::new(&contents);
            loop {
                match lexer.next_token() {
                    Ok((token, span)) => {
                        if token == Token::Eof {
                            break;
                        };
                        println!(
                            "{}:{}:{}: {:?}",
                            file, span.start.line, span.end.column, token
                        );
                    }
                    Err(err) => return Err(Error::Syntax(file.clone(), contents, Box::new(err))),
                }
            }
        }

        Some(("nodes", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut ctx = Context::new();
            let nodes = match parse(&mut ctx, &contents) {
                Ok(nodes) => nodes,
                Err(err) => return Err(Error::Syntax(file.clone(), contents, Box::new(err))),
            };

            for node in nodes {
                println!("{}", node.display(&ctx));
            }
        }

        Some(("type", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut ctx = Context::new();
            let nodes = match parse(&mut ctx, &contents) {
                Ok(nodes) => nodes,
                Err(err) => return Err(Error::Syntax(file.clone(), contents, Box::new(err))),
            };

            if let Err(err) = infer(&mut ctx, &nodes) {
                return Err(Error::Type(file.clone(), contents, Box::new(err)));
            };

            for node in nodes {
                match ctx.get_node(node) {
                    Node::Expr(_) => println!("{} : {}", node.display(&ctx), ctx.get_type(node)),
                    Node::Bind(name, _) => println!("{} : {}", name, ctx.get_type(node)),
                }
            }
        }

        Some(("eval", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut ctx = Context::new();
            let nodes = match parse(&mut ctx, &contents) {
                Ok(nodes) => nodes,
                Err(err) => return Err(Error::Syntax(file.clone(), contents, Box::new(err))),
            };

            if let Err(err) = infer(&mut ctx, &nodes) {
                return Err(Error::Type(file.clone(), contents, Box::new(err)));
            };

            let value = match evaluate(&ctx, &nodes) {
                Ok(value) => value,
                Err(err) => return Err(Error::Eval(file.clone(), contents, err)),
            };

            println!("{}", value.display(&ctx));
        }

        Some(("repl", sub_matches)) => {
            let mut rl = DefaultEditor::new().expect("could not initialize line editor");
            let mut ctx = Context::new();
            let mut nodes = Vec::new();

            if let Some(file) = sub_matches.get_one::<String>("file") {
                let contents = fs::read_to_string(file)?;
                nodes.extend(match parse(&mut ctx, &contents) {
                    Ok(nodes) => nodes,
                    Err(err) => return Err(Error::Syntax(file.clone(), contents, Box::new(err))),
                });
                if let Err(err) = infer(&mut ctx, &nodes) {
                    return Err(Error::Type(file.clone(), contents, Box::new(err)));
                };
            }

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
                    Ok(nodes) => nodes,
                    Err(err) => {
                        match err {
                            SyntaxError::InvalidToken(span) => {
                                println!(
                                    "{}:{}: invalid token",
                                    span.start.line, span.start.column
                                );
                            }
                            SyntaxError::UnterminatedChar(span) => {
                                println!(
                                    "{}:{}: unterminated char",
                                    span.start.line, span.start.column
                                )
                            }
                            SyntaxError::UnterminatedString(span) => {
                                println!(
                                    "{}:{}: unterminated string",
                                    span.start.line, span.start.column
                                )
                            }
                            SyntaxError::EmptyChar(span) => {
                                println!("{}:{}: empty char", span.start.line, span.start.column)
                            }
                            SyntaxError::InvalidChar(span) => {
                                println!("{}:{}: invalid char", span.start.line, span.start.column)
                            }
                            SyntaxError::InvalidEscapeChar(span) => {
                                println!(
                                    "{}:{}: invalid escape char",
                                    span.start.line, span.start.column
                                )
                            }
                            SyntaxError::UnexpectedToken(expect, (token, span)) => {
                                println!(
                                    "{}:{}: expected '{expect}' but got '{token}'",
                                    span.start.line, span.start.column
                                );
                            }
                        }
                        continue;
                    }
                });

                if let Err(err) = infer(&mut ctx, &nodes) {
                    match err {
                        type_system::Error::TypeMismatch((t1, s1), (t2, s2)) => {
                            println!(
                                "{}:{}: type mismatch: expected '{t1}'",
                                s1.start.line, s1.start.column
                            );
                            println!("{}:{}: but found '{t2}'", s2.start.line, s2.start.column);
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
                        type_system::Error::NoInstance(cons, ty, span) => {
                            println!(
                                "{}:{}: No '{cons}' instance for type '{ty}'",
                                span.start.line, span.start.column
                            );
                        }
                    }
                    nodes.pop();
                    continue;
                };

                if let Node::Expr(_) = ctx.get_node(*nodes.last().unwrap()) {
                    let value = match evaluate(&ctx, &nodes) {
                        Ok(value) => value,
                        Err(err) => {
                            match err {
                                eval::Error::DivisionByZero(span) => println!(
                                    "{}:{}: division by zero",
                                    span.start.line, span.start.column
                                ),
                                eval::Error::EmptyList(span) => println!(
                                    "{}:{}: list is empty",
                                    span.start.line, span.start.column
                                ),
                                eval::Error::IOError(message, span) => {
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
        }

        _ => unreachable!(),
    };

    Ok(())
}

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
                eprint!("{error}");
                std::process::exit(1);
            }
        }
    }
}
