mod lexer;

use crate::lexer::tokenize;
use std::{env::args, fs::read_to_string, process::exit};

fn main() {
    let args: Vec<String> = args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        exit(1);
    }
    let filename = &args[1];

    match read_to_string(filename) {
        Ok(contents) => {
            let tokens = tokenize(&contents).unwrap();
            for token in tokens {
                println!(
                    "{}:{}:{}: {:?}",
                    filename, token.span.start.line, token.span.start.column, token.kind
                )
            }
        }
        Err(e) => {
            eprintln!("Error reading file '{}': {}", filename, e);
            exit(1);
        }
    }
}
