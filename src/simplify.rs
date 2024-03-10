use std::mem::replace;

use crate::Gate;

macro_rules! replace {
    ($root:ident, $new:expr) => {{
        let new = replace($new, Self::Is(0));
        *$root = new;
    }};
}

impl Gate {
    pub fn simplify(&mut self) {
        self.map(&mut |root| match root {
            Self::Not(box Self::Not(box gate)) => replace!(root, gate),
            Self::Or(box (lhs, Self::Not(box rhs))) if lhs == rhs => *root = Self::Literal(true),
            Self::Or(box (lhs, rhs)) if lhs == rhs => replace!(root, lhs),
            _ => {}
        });
    }
}
