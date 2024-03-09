#![allow(clippy::eq_op, clippy::wildcard_imports)]

mod gates;
mod parser;

use std::process::ExitCode;

pub use gates::*;

use crate::parser::parse;

fn main() -> ExitCode {
    let Some(input) = std::env::args().nth(1) else {
        return ExitCode::FAILURE;
    };
    let tree = match parse(&input) {
        Ok(tree) => tree,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };
    tree.print_table();
    ExitCode::SUCCESS
}
