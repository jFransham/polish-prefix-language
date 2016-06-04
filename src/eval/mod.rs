use std::rc::Rc;
use ast::Expr;

mod value;
mod scope;

use self::value::Memo;
use self::scope::MutableScope;

pub use self::scope::Scope;
pub use self::value::Value;

macro_rules! hash {
    ($e:expr) => {{
        use std::hash::{SipHasher, Hash, Hasher};

        let mut hasher = SipHasher::new();

        ($e).hash(&mut hasher);

        hasher.finish()
    }};
}

macro_rules! val_true { () => { Value::Hash(hash!("t")) }; }
macro_rules! val_false { () => { Value::Hash(hash!("f")) }; }

enum Action {
    DefineVariable(String, Expr),
    DefineRecUnary(String, String, Expr, bool),
    DefineRecBinary(String, String, String, Expr, bool),
    Eval(Expr),
}

fn eval_macro_def(args: Vec<Expr>, memo: bool) -> Action {
    use ast::Expr::*;
    use self::Action::*;

    let mut iter = args.into_iter();

    match (iter.next(), iter.next(), iter.next()) {
        (
            Some(UnaryFnCall(box Variable(n), box Variable(arg))),
            Some(val),
            None,
        ) =>
            DefineRecUnary(
                n,
                arg,
                val,
                memo
            ),
        (
            Some(
                BinaryFnCall(
                    box Variable(n),
                    box Variable(arg_0),
                    box Variable(arg_1),
                )
            ),
            Some(val),
            None,
        ) =>
            DefineRecBinary(
                n,
                arg_0,
                arg_1,
                val,
                memo
            ),
        (
            Some(Variable(n)),
            Some(val),
            None,
        ) =>
            DefineVariable(n, val),
        _ => unimplemented!(),
    }
}

fn eval_macro_fn(args: Vec<Expr>, memo: bool) -> Action {
    use ast::Expr::*;
    use self::Action::*;

    let mut iter = args.into_iter();

    match (iter.next(), iter.next(), iter.next(), iter.next()) {
        (Some(Variable(arg)), Some(val), None, None) =>
            Eval(UnaryFn(arg, box val, memo)),
        (
            Some(Variable(arg_0)),
            Some(Variable(arg_1)),
            Some(val),
            None
        ) =>
            Eval(BinaryFn(arg_0, arg_1, box val, memo)),
        _ => unimplemented!(),
    }
}

fn eval_macro(macro_name: &str, args: Vec<Expr>) -> Action {
    use self::Action::*;

    match macro_name {
        "def" => eval_macro_def(args, false),
        "memdef" => eval_macro_def(args, true),
        "fn" => eval_macro_fn(args, false),
        "memfn" => eval_macro_fn(args, true),
        "if" => {
            use ast::Expr::*;

            let mut iter = args.into_iter();

            match (iter.next(), iter.next(), iter.next(), iter.next()) {
                (Some(cond), Some(then), None, None) =>
                    Eval(
                        If(
                            box cond,
                            box then,
                            box Lit(Value::List(vec![]))
                        )
                    ),
                (Some(cond), Some(then), Some(el), None) =>
                    Eval(If(box cond, box then, box el)),
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

pub fn eval(expression: &Expr, variables: Scope) -> Rc<Value> {
    use self::Action::*;

    match *expression {
        Expr::Lit(ref v) => v.clone().into(),
        Expr::Macro(ref name, ref args) =>
            match eval_macro(name, args.clone()) {
                Eval(e) => eval(&e, variables),
                _       => unimplemented!(),
            },
        Expr::Block(ref exprs) => {
            let mut out = Rc::new(Value::List(vec![]));

            let mut inner_scope = variables;

            for ex in exprs {
                out = match *ex {
                    Expr::Macro(ref name, ref args) =>
                        match eval_macro(name, args.clone()) {
                            Eval(e) => eval(
                                &e,
                                inner_scope.clone(),
                            ),
                            DefineVariable(name, var) => {
                                let val = eval(
                                    &var,
                                    inner_scope.clone(),
                                );

                                inner_scope = inner_scope.with_var(&name, val);

                                Rc::new(Value::List(vec![]))
                            },
                            DefineRecUnary(name, arg, var, memo) => {
                                let fun: Rc<_> = Value::UnaryFn(
                                    arg,
                                    Rc::new(var),
                                    MutableScope::new(inner_scope.clone()),
                                    if memo {
                                        Memo::new()
                                    } else {
                                        Memo::empty()
                                    },
                                ).into();

                                inner_scope = inner_scope.with_var(
                                    &name,
                                    fun.clone()
                                );

                                if let Value::UnaryFn(
                                    _,
                                    _,
                                    ref m_scope,
                                    _
                                ) = *fun {
                                    m_scope.set(inner_scope.clone());
                                }

                                Rc::new(Value::List(vec![]))
                            },
                            DefineRecBinary(name, arg_0, arg_1, var, memo) => {
                                let fun: Rc<_> = Value::BinaryFn(
                                    arg_0,
                                    arg_1,
                                    Rc::new(var),
                                    MutableScope::new(inner_scope.clone()),
                                    if memo {
                                        Memo::new()
                                    } else {
                                        Memo::empty()
                                    },
                                ).into();

                                inner_scope = inner_scope.with_var(
                                    &name,
                                    fun.clone()
                                );

                                if let Value::BinaryFn(
                                    _,
                                    _,
                                    _,
                                    ref m_scope,
                                    _,
                                ) = *fun {
                                    m_scope.set(inner_scope.clone());
                                }

                                Rc::new(Value::List(vec![]))
                            },
                        },
                    ref any => eval(any, inner_scope.clone()),
                };
            }

            out
        },
        Expr::UnaryFnCall(ref exp, ref arg) => {
            let func = eval(exp, variables.clone());
            let parameter = eval(arg, variables.clone());

            match *func {
                Value::UnaryFn(
                    ref param_name,
                    ref exp,
                    ref captured,
                    ref memo,
                ) => {
                    if let Some(v) = memo.get(&parameter) {
                        v
                    } else {
                        let inner_scope = captured.borrow().with_var(
                            param_name,
                            parameter.clone()
                        );

                        let out = eval(exp, inner_scope);

                        memo.insert(parameter, out.clone());

                        out
                    }
                },
                Value::BuiltinUnaryFn(ref f) => f(parameter),
                ref any => panic!("Expected unary fn, found {}.", any),
            }
        },
        Expr::BinaryFnCall(ref exp, ref arg_0, ref arg_1) => {
            let func = eval(exp, variables.clone());
            let params = (
                eval(arg_0, variables.clone()),
                eval(arg_1, variables.clone()),
            );

            match *func {
                Value::BinaryFn(
                    ref param_name_0,
                    ref param_name_1,
                    ref exp,
                    ref captured,
                    ref memo,
                ) => {
                    if let Some(v) = memo.get(&params) {
                        v
                    } else {
                        let inner_scope = captured.borrow()
                            .with_var(param_name_0, params.0.clone())
                            .with_var(param_name_1, params.1.clone());

                        let out = eval(exp, inner_scope);

                        memo.insert(params, out.clone());

                        out
                    }
                },
                Value::BuiltinBinaryFn(ref f) => f(params.0, params.1),
                ref any => panic!("Expected binary fn, found {}.", any),
            }
        },
        Expr::If(ref cond, ref a, ref b) => {
            let c = eval(cond, variables.clone());

            if val_true!() == *c {
                eval(a, variables)
            } else {
                eval(b, variables)
            }
        },
        Expr::UnaryFn(ref param_name, ref body, ref memo) =>
            Value::UnaryFn(
                param_name.clone(),
                Rc::new(*body.clone()),
                MutableScope::new(variables.clone()),
                if *memo { Memo::new() } else { Memo::empty() },
            ).into(),
        Expr::BinaryFn(ref p_name_0, ref p_name_1, ref body, ref memo) =>
            Value::BinaryFn(
                p_name_0.clone(),
                p_name_1.clone(),
                Rc::new(*body.clone()),
                MutableScope::new(variables.clone()),
                if *memo { Memo::new() } else { Memo::empty() },
            ).into(),
        Expr::Variable(ref name) => variables.get(hash!(name)).expect(
            &format!("Cannot find variable {}", name)
        ),
        Expr::List(ref exprs) => Value::List(
            exprs.iter().map(
                |e| eval(e, variables.clone())
            ).collect()
        ).into(),
    }
}

#[allow(match_ref_pats)]
pub fn stdlib() -> Scope {
    use self::Value::*;

    macro_rules! make_is_fn {
        ($name:ident, $( $pattern:pat )|+) => {
            fn $name(val: Rc<Value>) -> Rc<Value> {
                $(
                    if let $pattern = *val {
                        return val_true!().into();
                    }
                )+

                val_false!().into()
            }
        }
    }

    make_is_fn!(is_str, Str(_));
    make_is_fn!(is_int, Int(_));
    make_is_fn!(is_hash, Hash(_));
    make_is_fn!(
        is_fn,
        UnaryFn(..) | BinaryFn(..) | BuiltinUnaryFn(..) | BuiltinBinaryFn(..)
    );

    fn print(val: Rc<Value>) -> Rc<Value> {
        if let Str(ref s) = *val {
            println!("{}", s)
        } else {
            println!("{}", val);
        }

        List(vec![]).into()
    }

    fn add(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        match (a.as_ref(), b.as_ref()) {
            (&Int(ref i_a), &Int(ref i_b)) => Int(i_a + i_b).into(),
            _ => unimplemented!(),
        }
    }

    fn modulo(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        match (a.as_ref(), b.as_ref()) {
            (&Int(ref i_a), &Int(ref i_b)) => Int(i_a % i_b).into(),
            _ => unimplemented!(),
        }
    }

    fn mul(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        match (a.as_ref(), b.as_ref()) {
            (&Int(ref i_a), &Int(ref i_b)) => Int(i_a * i_b).into(),
            _ => unimplemented!(),
        }
    }

    fn div(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        match (a.as_ref(), b.as_ref()) {
            (&Int(ref i_a), &Int(ref i_b)) => Int(i_a / i_b).into(),
            _ => unimplemented!(),
        }
    }

    fn cons(a_rc: Rc<Value>, b_rc: Rc<Value>) -> Rc<Value> {
        match (a_rc, b_rc.as_ref()) {
            (ref a, &List(ref v)) => {
                List(Some(a.clone()).into_iter().chain(
                    v.iter().cloned()
                ).collect()).into()
            },
            (ref a, _) => {
                List(vec![a.clone(), b_rc.clone()]).into()
            },
        }
    }

    fn head(a: Rc<Value>) -> Rc<Value> {
        match a.as_ref() {
            &List(ref v) => {
                v.iter().next().cloned().unwrap_or_else(
                    || Rc::new(List(vec![]))
                )
            },
            _ => unimplemented!(),
        }
    }

    fn tail(a: Rc<Value>) -> Rc<Value> {
        match a.as_ref() {
            &List(ref l) => {
                List(l[1..].iter().cloned().collect()).into()
            },
            _ => unimplemented!(),
        }
    }

    fn eq(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        if a == b {
            val_true!().into()
        } else {
            val_false!().into()
        }
    }

    fn less_or_eq(a: Rc<Value>, b: Rc<Value>) -> Rc<Value> {
        if let (&Int(ref a_i), &Int(ref b_i)) = (a.as_ref(), b.as_ref()) {
            if a_i <= b_i {
                val_true!().into()
            } else {
                val_false!().into()
            }
        } else {
            val_false!().into()
        }
    }

    fn neg(a: Rc<Value>) -> Rc<Value> {
        match a.as_ref() {
            &Int(ref i_a) => Int(-i_a).into(),
            _ => unimplemented!(),
        }
    }

    let mut out = Scope::new_with_var(
        "print",
        Value::BuiltinUnaryFn(print).into()
    );

    out = out.with_var("=", Value::BuiltinBinaryFn(eq).into())
        .with_var("<=", Value::BuiltinBinaryFn(less_or_eq).into())
        .with_var("+", Value::BuiltinBinaryFn(add).into())
        .with_var("*", Value::BuiltinBinaryFn(mul).into())
        .with_var("/", Value::BuiltinBinaryFn(div).into())
        .with_var("%", Value::BuiltinBinaryFn(modulo).into())
        .with_var("cons", Value::BuiltinBinaryFn(cons).into())
        .with_var("-", Value::BuiltinUnaryFn(neg).into())
        .with_var("head", Value::BuiltinUnaryFn(head).into())
        .with_var("tail", Value::BuiltinUnaryFn(tail).into())
        .with_var("str?", Value::BuiltinUnaryFn(is_str).into())
        .with_var("int?", Value::BuiltinUnaryFn(is_int).into())
        .with_var("hash?", Value::BuiltinUnaryFn(is_hash).into())
        .with_var("fn?", Value::BuiltinUnaryFn(is_fn).into());

    out
}
