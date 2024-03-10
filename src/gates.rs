use std::{
    fmt::{self, Write},
    ops::{Add, BitAnd, BitOr, BitXor, Mul},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Gate {
    Literal(bool),
    Is(u8),
    Not(Box<Gate>),
    And(Box<(Gate, Gate)>),
    Or(Box<(Gate, Gate)>),
    Xor(Box<(Gate, Gate)>),
}

impl Gate {
    #[must_use]
    pub fn compute(&self, val: u32) -> bool {
        match self {
            Self::Literal(bool) => *bool,
            Self::Is(index) => ((val >> index) & 1) == 1,
            Self::Not(gate) => !gate.compute(val),
            Self::And(gate) => gate.0.compute(val) && gate.1.compute(val),
            Self::Or(gate) => gate.0.compute(val) || gate.1.compute(val),
            Self::Xor(gate) => gate.0.compute(val) ^ gate.1.compute(val),
        }
    }

    pub fn map(&mut self, predicate: &mut impl FnMut(&mut Self)) {
        match self {
            Self::And(gates) | Self::Or(gates) | Self::Xor(gates) => {
                predicate(&mut gates.0);
                predicate(&mut gates.1);
            }
            Self::Not(gate) => gate.map(predicate),
            Self::Is(_) | Self::Literal(_) => {}
        }
        predicate(self);
    }

    pub fn reverse(&mut self, depth: Option<u8>) {
        if depth == Some(0) {
            return;
        }

        match self {
            Self::And(gates) | Self::Or(gates) | Self::Xor(gates) => {
                gates.0.reverse(depth.map(|d| d - 1));
                gates.1.reverse(depth.map(|d| d - 1));
                std::mem::swap(&mut gates.0, &mut gates.1);
            }
            Self::Not(gate) => gate.reverse(depth.map(|d| d - 1)),
            Self::Is(_) | Self::Literal(_) => {}
        }
    }

    #[must_use]
    pub fn size(&self) -> u32 {
        let mut bits: u32 = 0;
        self.vars(&mut |x| bits |= 1 << x);
        bits.count_ones()
    }

    pub fn unique_vars(&self) -> impl Iterator<Item = u8> {
        let mut bits: u32 = 0;
        self.vars(&mut |x| bits |= 1 << x);
        (0..32).filter(move |index| ((bits >> index) & 1) == 1)
    }

    pub fn vars(&self, with: &mut impl FnMut(u32)) {
        match self {
            Self::Literal(_) => {}
            Self::Is(index) => with(*index as u32),
            Self::Not(gate) => gate.vars(with),
            Self::And(gate) | Self::Or(gate) | Self::Xor(gate) => {
                gate.0.vars(with);
                gate.1.vars(with);
            }
        }
    }

    #[must_use]
    pub fn equal(&self, other: &Self) -> bool {
        let (lhs_size, rhs_size) = (self.size(), other.size());
        if lhs_size < rhs_size {
            return false;
        }
        (0..2u32.pow(lhs_size.max(rhs_size))).all(|val| self.compute(val) == other.compute(val))
    }

    pub fn print_table(&self) {
        let width = self.size();
        for _ in 0..=width + 1 {
            print!("--");
        }
        println!();
        print!("|");
        for var in self.unique_vars() {
            print!("{} ", (b'A' + var) as char);
        }
        print!("  |");
        println!();
        for i in 0..2u32.pow(width) {
            let width = width as usize;
            let bits = format!("{i:0width$b}").chars().rev().flat_map(|c| [c, ' ']).collect::<String>();
            println!("|{bits} {}|", u32::from(self.compute(i)));
        }
        for _ in 0..=width + 1 {
            print!("--");
        }
        println!();
    }

    #[must_use]
    pub fn count_ones(&self) -> u32 {
        let mut num = 0;
        for i in 0..2u32.pow(self.size()) {
            num += u32::from(self.compute(i));
        }
        num
    }
}

impl std::ops::Not for Gate {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self::Not(Box::new(self))
    }
}

macro_rules! impl_op {
    ($gate:tt, $op:tt, $func:tt) => {
        impl $op for Gate {
            type Output = Self;

            fn $func(self, rhs: Self) -> Self {
                Self::$gate(Box::new((self, rhs)))
            }
        }
    };
}

impl_op!(And, BitAnd, bitand);
impl_op!(And, Mul, mul);
impl_op!(Or, BitOr, bitor);
impl_op!(Or, Add, add);
impl_op!(Xor, BitXor, bitxor);

pub mod consts {
    use super::Gate;
    pub const A: Gate = Gate::Is(0);
    pub const B: Gate = Gate::Is(1);
    pub const C: Gate = Gate::Is(2);
    pub const D: Gate = Gate::Is(3);
    pub const E: Gate = Gate::Is(4);
    pub const F: Gate = Gate::Is(5);
    pub const G: Gate = Gate::Is(6);
    pub const H: Gate = Gate::Is(7);
    pub const I: Gate = Gate::Is(8);
    pub const J: Gate = Gate::Is(9);
    pub const K: Gate = Gate::Is(10);
    pub const L: Gate = Gate::Is(11);
    pub const M: Gate = Gate::Is(12);
    pub const N: Gate = Gate::Is(13);
    pub const O: Gate = Gate::Is(14);
    pub const P: Gate = Gate::Is(15);
    pub const Q: Gate = Gate::Is(16);
    pub const R: Gate = Gate::Is(17);
    pub const S: Gate = Gate::Is(18);
    pub const T: Gate = Gate::Is(19);
    pub const U: Gate = Gate::Is(20);
    pub const V: Gate = Gate::Is(21);
    pub const W: Gate = Gate::Is(22);
    pub const X: Gate = Gate::Is(23);
    pub const Y: Gate = Gate::Is(24);
    pub const Z: Gate = Gate::Is(25);
}

impl fmt::Display for Gate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Literal(bool) => write!(f, "{}", *bool as u8),
            Self::Not(gate) => write!(f, "!{gate}"),
            Self::And(gate) => write!(f, "({}.{})", gate.0, gate.1),
            Self::Or(gate) => write!(f, "({}+{})", gate.0, gate.1),
            Self::Xor(gate) => write!(f, "({}^{})", gate.0, gate.1),
            Self::Is(index) => f.write_char((b'A' + index) as char),
        }
    }
}
