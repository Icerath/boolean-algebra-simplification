#![allow(clippy::eq_op, clippy::wildcard_imports)]

mod gates;
mod parser;

use crate::parser::parse;
pub use gates::*;

fn main() {
    let Some(input) = std::env::args().nth(1) else {
        return;
    };
    let tree = parse(&input).unwrap_or_else(|err| panic!("{err}"));
    tree.print_table();
}
