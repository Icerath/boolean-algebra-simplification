use std::ops::{Add, BitAnd, BitOr, BitXor, Mul};

#[derive(Debug)]
pub enum Gate {
    Is(u32),
    Not(Box<Gate>),
    And(Box<(Gate, Gate)>),
    Or(Box<(Gate, Gate)>),
    Xor(Box<(Gate, Gate)>),
}

impl Gate {
    #[must_use]
    pub fn compute(&self, val: u32) -> bool {
        match self {
            Self::Is(index) => ((val >> index) & 1) == 1,
            Self::Not(gate) => !gate.compute(val),
            Self::And(gate) => gate.0.compute(val) && gate.1.compute(val),
            Self::Or(gate) => gate.0.compute(val) || gate.1.compute(val),
            Self::Xor(gate) => gate.0.compute(val) ^ gate.1.compute(val),
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
            Self::Is(index) => with(*index),
            Self::Not(gate) => gate.vars(with),
            Self::And(gate) | Self::Or(gate) | Self::Xor(gate) => {
                gate.0.vars(with);
                gate.1.vars(with);
            }
        }
    }
    pub fn print_table(&self) {
        let vars = self.unique_vars().collect::<Vec<_>>();
        let width = vars.len();
        for var in &vars {
            print!("{} ", (b'A' + var) as char);
        }
        println!();
        for i in 0..2u32.pow(width as u32) {
            let bits = format!("{i:0width$b}")
                .chars()
                .rev()
                .flat_map(|c| [c, ' '])
                .collect::<String>();
            println!("{bits} {}", u32::from(self.compute(i)));
        }
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
    type Output = Gate;
    fn not(self) -> Self::Output {
        Gate::Not(Box::new(self))
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
