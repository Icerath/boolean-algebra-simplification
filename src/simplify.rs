use crate::Gate;

macro_rules! replace {
    ($root:ident, $new:expr) => {{
        let new = std::mem::replace($new, Gate::Is(0));
        *$root = new;
    }};
}

macro_rules! set {
    ($root:ident, $literal:literal) => {
        *$root = Gate::Literal($literal == 1)
    };
}

impl Gate {
    pub fn simplify(&mut self) {
        self.map(&mut |root| {
            simplify(root);
            root.reverse(Some(1));
            simplify(root);
            root.reverse(Some(2));
            simplify(root);
            root.reverse(Some(1));
            simplify(root);
            root.reverse(Some(2));
        });
    }
}

pub fn simplify(root: &mut Gate) {
    use Gate::{And, Literal, Not, Or};
    match root {
        Not(box Not(box gate)) => replace!(root, gate),
        // Identity Law
        And(box (Literal(true), other)) | Or(box (Literal(false), other)) => replace!(root, other),
        // Null Law
        And(box (Literal(null @ false), _)) | Or(box (Literal(null @ true), _)) => *root = Literal(*null),
        // Idempotent law
        And(box (lhs, rhs)) | Or(box (lhs, rhs)) if lhs == rhs => replace!(root, lhs),
        // Inverse Law
        And(box (lhs, Not(box rhs))) if lhs == rhs => set!(root, 0),
        Or(box (lhs, Not(box rhs))) if lhs == rhs => set!(root, 1),
        //
        Or(box (a1, And(box (a2, _b)))) if a1 == a2 => replace!(root, a1),
        //
        Or(box (lhs, And(box (Not(box rhs_a), rhs_b)))) if lhs == rhs_a => {
            let mut new = lhs.clone() + rhs_b.clone();
            replace!(root, &mut new);
        }
        And(box (Or(box (a1, b)), Or(box (a2, c)))) if a1 == a2 => {
            let mut new = a1.clone() + (b.clone() & c.clone());
            replace!(root, &mut new);
        }
        _ => {}
    }
}

#[cfg(test)]
macro_rules! test_simplified {
    ($lhs:expr, $rhs:expr) => {
        let mut lhs = $lhs;
        let rhs = $rhs;
        lhs.simplify();
        assert_eq!(lhs, rhs);
        assert!(lhs.equal(&rhs));
    };
}

#[allow(clippy::cognitive_complexity)]
#[test]
fn test_simplification() {
    use Gate::Literal;

    use crate::gates::consts::*;

    test_simplified!(A + B, A + B);
    test_simplified!(A + Literal(false), A);
    test_simplified!(A + Literal(true), Literal(true));
    test_simplified!(A & Literal(false), Literal(false));
    test_simplified!(A & Literal(true), A);
    test_simplified!(A + A, A);
    test_simplified!(!A + A, Literal(true));
    test_simplified!(A & A, A);
    test_simplified!(!A & A, Literal(false));
    test_simplified!(!!A, A);
    test_simplified!((A & B) + A, A);
    test_simplified!(A + (!A & B), A + B);
    test_simplified!((A + B) & (A + C), A + (B & C));
}
