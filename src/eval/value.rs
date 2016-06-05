use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;
use std::cell::RefCell;
use std::hash::{Hash, Hasher};
use num_bigint::BigInt;

use eval::scope::*;
use eval::{Pattern, Args};
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
pub struct Memo(Option<RefCell<HashMap<Args, Rc<Value>>>>);

impl Memo {
    pub fn new() -> Self {
        Memo(Some(RefCell::new(HashMap::new())))
    }

    pub fn empty() -> Self {
        Memo(None)
    }

    pub fn get(&self, value: &Args) -> Option<Rc<Value>> {
        if let Some(ref refcell) = self.0 {
            (*refcell.borrow()).get(value).cloned()
        } else {
            None
        }
    }

    pub fn insert(&self, input: Args, output: Rc<Value>) {
        if let Some(ref refcell) = self.0 {
            (*refcell.borrow_mut()).insert(input, output);
        }
    }
}

impl Default for Memo {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(BigInt),
    Str(String),
    Hash(u64),
    Mutable(RefCell<Rc<Value>>),
    Func(Pattern, Rc<Expr>, MutableScope, Memo),
    BuiltinFunc(fn(Args) -> Rc<Value>),
    List(Vec<Rc<Value>>),
}

impl Hash for Value {
    fn hash<T: Hasher>(&self, hasher: &mut T) {
        use self::Value::*;

        match *self {
            Int(ref a) => a.hash(hasher),
            Str(ref a) => a.hash(hasher),
            Hash(ref a) => a.hash(hasher),
            Func(ref pat, ref e, ref scope, _) => {
                pat.hash(hasher);
                e.hash(hasher);
                scope.hash(hasher);
            },
            BuiltinFunc(ref a) => a.hash(hasher),
            List(ref a) => a.hash(hasher),
            Mutable(ref a) => a.borrow().hash(hasher),
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
                &Func(ref param_a, ref e_a, ref scope_a, _),
                &Func(ref param_b, ref e_b, ref scope_b, _),
            ) =>
                param_a == param_b && e_a == e_b && scope_a == scope_b,
            (&BuiltinFunc(ref a), &BuiltinFunc(ref b)) => a == b,
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
            Func(..) =>
                write!(f, "($fn ...)"),
            BuiltinFunc(..) =>
                write!(f, "($fn {{{{builtin}}}})"),
            Mutable(ref inner) =>
                write!(f, "mut.{}", &*inner.borrow()),
        }
    }
}
