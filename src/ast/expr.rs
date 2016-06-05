use std::rc::Rc;
use std::fmt::{self, Display, Formatter};
use eval::{Pattern, Value};

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum Expr {
    UnaryFnCall(Box<Expr>, Box<Expr>),
    BinaryFnCall(Box<Expr>, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    // bool = whether to memoise. Ideally in the future this would be
    // implemented using $set and script-space macros, but neither is
    // implemented yet, so we have this mildly rancid hack.
    // TODO: unrancid this implementation
    // XXX: Needs:
    //      * script-space representation of AST (for macros).
    //      * $set macro (needs to replace Scope's value: Rc<Value> with
    //        value: RefCell<Rc<Value>>.
    Func(Pattern, Box<Expr>, bool),
    Lit(Rc<Value>),
    Variable(String),
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Macro(String, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        use ast::Expr::*;

        match *self {
            UnaryFnCall(ref func, ref arg) =>
                write!(f, "(({}):{})", func, arg),
            BinaryFnCall(ref func, ref arg_0, ref arg_1) =>
                write!(f, "(({}): {} {})", func, arg_0, arg_1),
            Func(_, _, ref memo) =>
                write!(
                    f,
                    "($fn {}...)",
                    if *memo { "#memo " } else { "" }
                ),
            If(ref cond, ref then, ref el) =>
                write!(f, "($if {} {} {})", cond, then, el),
            Lit(ref val) =>
                write!(f, "{}", val),
            Variable(ref var) =>
                write!(f, "{}", var),
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
            Block(ref list) => {
                write!(f, "(")?;

                let mut iter = list.into_iter();

                if let Some(v) = iter.next() {
                    write!(f, "{}", v)?;
                }

                for v in iter {
                    write!(f, "; {}", v)?;
                }

                write!(f, ")")?;

                Ok(())
            },
            Macro(ref name, _) => write!(f, "(${} ...)", name),
        }
    }
}
