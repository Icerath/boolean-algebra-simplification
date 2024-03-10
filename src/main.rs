#![feature(box_patterns)]
#![allow(clippy::eq_op, clippy::wildcard_imports)]

mod gates;
mod parser;
pub mod simplify;

pub use gates::*;

use crate::parser::parse;

#[derive(clap::Parser)]
struct Args {
    input: String,
    #[arg(long, default_value = "false")]
    truth_table: bool,
}

fn main() -> Result<(), String> {
    let args = <Args as clap::Parser>::parse();
    let og_tree = parse(&args.input).map_err(|err| err.to_string())?;
    let mut new_tree = og_tree.clone();
    new_tree.simplify();
    if args.truth_table {
        og_tree.print_table();
        if og_tree.size() != new_tree.size() {
            new_tree.print_table();
        }
    }
    println!("old = {og_tree}");
    println!("new = {new_tree}");
    assert!(og_tree.equal_truth_tables(&new_tree));
    Ok(())
}
