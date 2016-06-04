use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use num_bigint::BigInt;

use eval::scope::*;
use ast::Expr;

macro_rules! hash {
    ($e:expr) => {{
        use std::hash::{SipHasher, Hash};

        let mut hasher = SipHasher::new();

        ($e).hash(&mut hasher);

        hasher.finish()
    }};
}

#[derive(Debug, Clone)]
pub struct Memo<T: Hash + Eq>(Option<RefCell<HashMap<T, Rc<Value>>>>);

pub type UnaryMemo = Memo<Rc<Value>>;
pub type BinaryMemo = Memo<(Rc<Value>, Rc<Value>)>;

impl<T: Hash + Eq> Memo<T> {
    pub fn new() -> Self {
        Memo(Some(RefCell::new(HashMap::new())))
    }

    pub fn empty() -> Self {
        Memo(None)
    }

    pub fn get(&self, value: &T) -> Option<Rc<Value>> {
        if let Some(ref refcell) = self.0 {
            (*refcell.borrow()).get(value).cloned()
        } else {
            None
        }
    }

    pub fn insert(&self, input: T, output: Rc<Value>) {
        if let Some(ref refcell) = self.0 {
            (*refcell.borrow_mut()).insert(input, output);
        }
    }
}

impl<T: Hash + Eq> Default for Memo<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(BigInt),
    Str(String),
    Hash(u64),
    UnaryFn(String, Rc<Expr>, MutableScope, UnaryMemo),
    BinaryFn(String, String, Rc<Expr>, MutableScope, BinaryMemo),
    BuiltinUnaryFn(fn(Rc<Value>) -> Rc<Value>),
    BuiltinBinaryFn(fn(Rc<Value>, Rc<Value>) -> Rc<Value>),
    List(Vec<Rc<Value>>),
}

impl Hash for Value {
    fn hash<T: Hasher>(&self, hasher: &mut T) {
        use self::Value::*;

        match *self {
            Int(ref a) => a.hash(hasher),
            Str(ref a) => a.hash(hasher),
            Hash(ref a) => a.hash(hasher),
            UnaryFn(ref param, ref e, ref scope, _) => {
                param.hash(hasher);
                e.hash(hasher);
                scope.hash(hasher);
            },
            BinaryFn(ref p0, ref p1, ref e, ref scope, _) => {
                p0.hash(hasher);
                p1.hash(hasher);
                e.hash(hasher);
                scope.hash(hasher);
            },
            BuiltinUnaryFn(ref a) => a.hash(hasher),
            BuiltinBinaryFn(ref a) => a.hash(hasher),
            List(ref a) => a.hash(hasher),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use self::Value::*;

        match (self, other) {
            (&Int(ref a), &Int(ref b)) => a == b,
            (&Str(ref a), &Str(ref b)) => a == b,
            (&Hash(ref a), &Hash(ref b)) => a == b,
            (
                &UnaryFn(ref param_a, ref e_a, ref scope_a, _),
                &UnaryFn(ref param_b, ref e_b, ref scope_b, _),
            ) =>
                param_a == param_b && e_a == e_b && scope_a == scope_b,
            (
                &BinaryFn(ref p0_a, ref p1_a, ref e_a, ref scope_a, _),
                &BinaryFn(ref p0_b, ref p1_b, ref e_b, ref scope_b, _),
            ) =>
                p0_a == p0_b &&
                p1_a == p1_b &&
                e_a == e_b &&
                scope_a == scope_b,
            (&BuiltinUnaryFn(ref a), &BuiltinUnaryFn(ref b)) => a == b,
            (&BuiltinBinaryFn(ref a), &BuiltinBinaryFn(ref b)) => a == b,
            (&List(ref a), &List(ref b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Value::*;

        match *self {
            Str(ref s) => write!(f, "\"{}\"", s),
            Int(ref i) => write!(f, "{}", i),
            List(ref list) => {
                write!(f, "[")?;

                let mut iter = list.into_iter();

                if let Some(v) = iter.next() {
                    write!(f, "{}", v)?;
                }

                for v in iter {
                    write!(f, " {}", v)?;
                }

                write!(f, "]")?;

                Ok(())
            },
            Hash(ref h) =>
                if *h == hash!("t") {
                    write!(f, "#t")
                } else if *h == hash!("f") {
                    write!(f, "#f")
                } else {
                    write!(f, "#{{{}}}", h)
                },
            UnaryFn(ref arg, ref expr, _, _) =>
                write!(f, "($fn {} {})", arg, expr),
            BinaryFn(ref arg_0, ref arg_1, ref expr, _, _) =>
                write!(f, "($fn {} {} {})", arg_0, arg_1, expr),
            BuiltinUnaryFn(..) =>
                write!(f, "($fn arg {{{{builtin}}}})"),
            BuiltinBinaryFn(..) =>
                write!(f, "($fn arg_0 arg_1 {{{{builtin}}}})"),
        }
    }
}
