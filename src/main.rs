#![feature(box_patterns)]
#![allow(clippy::eq_op, clippy::wildcard_imports)]

mod gates;
mod parser;
pub mod simplify;

use std::process::ExitCode;

pub use gates::*;

use crate::parser::parse;

fn main() -> ExitCode {
    let Some(input) = std::env::args().nth(1) else {
        return ExitCode::FAILURE;
    };
    let og_tree = match parse(&input) {
        Ok(tree) => tree,
        Err(err) => {
            eprintln!("{err}");
            return ExitCode::FAILURE;
        }
    };
    let mut new_tree = og_tree.clone();
    new_tree.simplify();
    println!("old = {og_tree}");
    println!("new = {new_tree}");
    assert!(og_tree.equal(&new_tree));
    ExitCode::SUCCESS
}
