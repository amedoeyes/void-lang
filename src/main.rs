mod context;
mod error;
mod eval;
mod expr;
mod lexer;
mod parser;
mod span;
mod type_system;

use clap::{Arg, Command, crate_name, crate_version};
use std::fs;

use crate::{
    context::{Context, Node},
    error::{Error, Result},
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
        .subcommand(Command::new("eval").arg(Arg::new("file").required(true).help("source file")));

    match cmd.try_get_matches()?.subcommand() {
        Some(("tokens", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut lexer = Lexer::new(&contents);
            while let (token, span) = lexer.next_token()
                && token != Token::Eof
            {
                println!(
                    "{}:{}:{}: {:?}",
                    file, span.start.line, span.start.column, token
                )
            }
        }

        Some(("nodes", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut ctx = Context::new();
            let nodes = match parse(&mut ctx, &contents) {
                Ok(nodes) => nodes,
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
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
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
            };

            if let Err(err) = infer(&mut ctx, &nodes) {
                return Err(Error::Type(file.clone(), contents, Box::new(err)));
            };

            for node in nodes {
                match ctx.get_node(node) {
                    Node::Expr(_) => println!("{}", ctx.get_type(node)),
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
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
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
