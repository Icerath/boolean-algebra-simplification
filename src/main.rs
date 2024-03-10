#![feature(box_patterns)]
#![allow(clippy::eq_op, clippy::wildcard_imports)]

mod gates;
mod parser;
pub mod simplify;

pub use gates::*;

use crate::parser::parse;

fn main() -> Result<(), String> {
    let Some(input) = std::env::args().nth(1) else {
        return Err("Expected Input".into());
    };
    let og_tree = parse(&input).map_err(|err| err.to_string())?;
    let mut new_tree = og_tree.clone();
    new_tree.simplify();
    println!("old = {og_tree}");
    println!("new = {new_tree}");
    assert!(og_tree.equal(&new_tree));
    Ok(())
}
