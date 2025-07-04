mod ast;
mod error;
mod eval;
mod lexer;
mod parser;
mod span;
mod type_system;

use clap::{Arg, Command, crate_name, crate_version, value_parser};
use clap_complete::{Shell, generate};
use std::{fs, io};

use crate::{
    error::{Error, Result},
    eval::evaluate,
    lexer::{Lexer, Token},
    parser::parse,
    type_system::infer,
};

fn run() -> Result<()> {
    let mut cmd = Command::new(crate_name!())
        .version(crate_version!())
        .disable_colored_help(true)
        .disable_help_subcommand(true)
        .arg_required_else_help(true)
        .subcommand_required(true)
        .subcommand(
            Command::new("tokens")
                .about("Dump tokens")
                .arg(Arg::new("file").required(true).help("source file")),
        )
        .subcommand(
            Command::new("ast")
                .about("Dump AST")
                .arg(Arg::new("file").required(true).help("source file")),
        )
        .subcommand(
            Command::new("type")
                .about("Dump typed AST")
                .arg(Arg::new("file").required(true).help("source file")),
        )
        .subcommand(
            Command::new("eval")
                .about("evaluate")
                .arg(Arg::new("file").required(true).help("source file")),
        )
        .subcommand(
            Command::new("completions")
                .about("Generate shell completions")
                .arg(
                    Arg::new("shell")
                        .value_parser(value_parser!(Shell))
                        .required(true),
                ),
        );

    let matches = cmd.clone().try_get_matches()?;

    if let Some(("completions", sub_matches)) = matches.subcommand() {
        let shell = sub_matches.get_one::<Shell>("shell").unwrap();
        let name = &cmd.get_name().to_string();
        generate(*shell, &mut cmd, name, &mut io::stdout());
        return Ok(());
    }

    match matches.subcommand() {
        Some(("tokens", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let mut lexer = Lexer::new(&contents);
            while let token = lexer.next_token()
                && token.value != Token::Eof
            {
                println!(
                    "{}:{}:{}: {:?}",
                    file, token.span.start.line, token.span.start.column, token.value
                )
            }
        }

        Some(("ast", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let ast = match parse(&contents) {
                Ok(ast) => ast,
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
            };

            println!("{ast:#?}");
        }

        Some(("type", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let ast = match parse(&contents) {
                Ok(ast) => ast,
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
            };

            let typed_ast = match infer(&ast) {
                Ok(typed_ast) => typed_ast,
                Err(err) => return Err(Error::Type(file.clone(), contents, Box::new(err))),
            };

            for node in typed_ast {
                match node.value.value {
                    ast::Stmt::Let { name, .. } => println!("{} : {}", name, node.ty),
                    ast::Stmt::Expr(_) => println!("{}", node.ty),
                }
            }
        }

        Some(("eval", sub_matches)) => {
            let file = sub_matches.get_one::<String>("file").unwrap();
            let contents = fs::read_to_string(file)?;

            let ast = match parse(&contents) {
                Ok(ast) => ast,
                Err(err) => return Err(Error::Parser(file.clone(), contents, Box::new(err))),
            };

            let typed_ast = match infer(&ast) {
                Ok(typed_ast) => typed_ast,
                Err(err) => return Err(Error::Type(file.clone(), contents, Box::new(err))),
            };

            let value = match evaluate(&typed_ast) {
                Ok(value) => value,
                Err(err) => return Err(Error::Eval(file.clone(), contents, err)),
            };

            println!("{value}");
        }

        _ => unreachable!(),
    };

    Ok(())
}

fn main() {
    if let Err(error) = run() {
        match error {
            Error::Clap(error) => match error.kind() {
                clap::error::ErrorKind::DisplayHelp
                | clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand
                | clap::error::ErrorKind::DisplayVersion => {
                    eprintln!("{error}");
                    std::process::exit(0);
                }
                _ => {
                    eprintln!("{error}");
                    std::process::exit(1);
                }
            },
            _ => {
                eprint!("{error}");
                std::process::exit(1);
            }
        }
    }
}
