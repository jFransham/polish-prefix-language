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

// TODO: use this to walk up the scope list until reaching a function that
//       matches the pattern.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Pattern {
    Unary(ArgPattern),
    Binary(ArgPattern, ArgPattern),
}

// TODO: lit pattern
// TODO: seperate this from function code? (maybe if this is seperate the
//       function code can be simpler? Can scripts use $def to emulate argument
//       deconstruction?)
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum ArgPattern {
    List(Vec<ArgPattern>, Option<Box<ArgPattern>>), // option is for ..rest
    // Expr must return a function that takes a single argument and returns
    // #f or #t (technically, anything that isn't #f evaluates to true, but
    // bite me).
    Pred(Rc<Expr>, Box<ArgPattern>),
    Var(String),
    Discard,
}

impl Pattern {
    fn deconstruct(&self, value: Args, scope: Scope) -> Option<Scope> {
        match *self {
            Pattern::Unary(ref p) =>
                if let Args::Unary(v) = value {
                    p.deconstruct(v, scope)
                } else {
                    None
                },
            Pattern::Binary(ref p0, ref p1) =>
                // TODO: Construct scopes seperately and then merge them
                // so that $fn a a:b ... doesn't work (it'd be silly).
                if let Args::Binary(v0, v1) = value {
                    p0.deconstruct(v0, scope).and_then(
                        |s| p1.deconstruct(v1, s)
                    )
                } else {
                    None
                },
        }
    }
}

impl ArgPattern {
    fn deconstruct(&self, value: Rc<Value>, scope: Scope) -> Option<Scope> {
        match *self {
            ArgPattern::Var(ref v) => Some(scope.with_var(v, value)),
            ArgPattern::List(ref vec, ref rest_pat) => {
                if let Value::List(ref vals) = *value {
                    if vals.len() < vec.len() { return None; }

                    let (values, rest) = vals.split_at(vec.len());

                    let val_scope = values.iter().zip(vec.iter()).fold(
                        Some(scope),
                        |scp, (val, pat)| scp.and_then(
                            |s| pat.deconstruct(val.clone(), s.clone())
                        )
                    );

                    if let Some(ref pat) = *rest_pat {
                        val_scope.and_then(
                            |scp| pat.deconstruct(
                                Value::List(
                                    rest.iter().cloned().collect()
                                ).into(),
                                scp
                            )
                        )
                    } else if rest.is_empty() {
                        val_scope
                    } else {
                        None
                    }
                } else {
                    None
                }
            },
            ArgPattern::Pred(ref p, ref pat) => {
                if *eval(
                    // TODO: Make this use Rc
                    &Expr::UnaryFnCall(
                        box p.as_ref().clone(),
                        box Expr::Lit(value.clone())
                    ), scope.clone()
                ) == val_false!() {
                    None
                } else {
                    pat.deconstruct(value, scope)
                }
            },
            ArgPattern::Discard => Some(scope),
        }
    }

    fn from_expr(expr: &Expr) -> Option<Self> {
        use ast::Expr::*;

        match *expr {
            UnaryFnCall(ref func, ref arg) => ArgPattern::from_expr(arg).map(
                |a| ArgPattern::Pred(
                    Rc::new((**func).clone()),
                    box a
                )
            ),
            Variable(ref name) =>
                if name == "_" {
                    Some(ArgPattern::Discard)
                } else {
                    Some(ArgPattern::Var(name.clone()))
                },
            List(ref exprs) => if exprs.is_empty() {
                Some(ArgPattern::List(vec![], None))
            } else {
                let last_index = exprs.len() - 1;

                let pat_slice = &exprs[..last_index];
                let last = &exprs[last_index];

                pat_slice.iter().map(ArgPattern::from_expr).fold(
                    Some(vec![]),
                    |total, current| match (total, current) {
                        (Some(mut tot), Some(curr)) => {
                            tot.push(curr);
                            Some(tot)
                        },
                        _ => None,
                    }
                ).and_then(
                    |mut out| match *last {
                        UnaryFnCall(box Variable(ref fun), ref pat_expr)
                            if fun == ".." => ArgPattern::from_expr(
                                pat_expr
                            ).map(
                                |pat| ArgPattern::List(
                                    out, Some(box pat)
                                )
                            ),
                        ref any => ArgPattern::from_expr(any).map(
                            |pat| {
                                out.push(pat);

                                ArgPattern::List(out, None)
                            }
                        ),
                    }
                )
            },
            _ => None,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Args {
    Unary(Rc<Value>),
    Binary(Rc<Value>, Rc<Value>),
}

enum Action {
    DefineVariable(String, Expr),
    DefineRecUnary(String, Expr, Expr, bool),
    DefineRecBinary(String, Expr, Expr, Expr, bool),
    Eval(Expr),
}

fn eval_macro_def(args: Vec<Expr>, memo: bool) -> Action {
    use ast::Expr::*;
    use self::Action::*;

    let mut iter = args.into_iter();

    match (iter.next(), iter.next(), iter.next()) {
        (
            Some(UnaryFnCall(box Variable(n), box arg)),
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
                    box arg_0,
                    box arg_1,
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
        (Some(arg), Some(val), None, None) =>
            Eval(
                Func(
                    Pattern::Unary(
                        ArgPattern::from_expr(&arg).expect("Invalid pattern")
                    ),
                    box val,
                    memo
                )
            ),
        (
            Some(arg_0),
            Some(arg_1),
            Some(val),
            None
        ) =>
            Eval(
                Func(
                    Pattern::Binary(
                        ArgPattern::from_expr(&arg_0).expect("Invalid pattern"),
                        ArgPattern::from_expr(&arg_1).expect("Invalid pattern"),
                    ),
                    box val,
                    memo
                )
            ),
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
                            box Lit(Value::List(vec![]).into())
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

fn eval_fn(
    (pat, body, scope, memo): (&Pattern, &Expr, &MutableScope, &Memo),
    params: Args
) -> Option<Rc<Value>> {
    if let Some(v) = memo.get(&params) {
        Some(v)
    } else if let Some(inner_scope) = pat.deconstruct(
        params.clone(),
        scope.borrow().clone()
    ) {
        let out = eval(body, inner_scope);

        memo.insert(params, out.clone());

        Some(out)
    } else {
        None
    }
}

fn mutate_scope_with_action(
    action: Action,
    scope: &mut Scope
) -> Rc<Value> {
    use self::Action::*;

    match action {
        Eval(e) => eval(
            &e,
            scope.clone(),
        ),
        DefineVariable(name, var) => {
            let val = eval(
                &var,
                scope.clone(),
            );

            *scope = scope.with_var(&name, val);

            Rc::new(Value::List(vec![]))
        },
        DefineRecUnary(name, arg, var, memo) => {
            let fun: Rc<_> = Value::Func(
                Pattern::Unary(ArgPattern::from_expr(&arg).unwrap()),
                Rc::new(var),
                MutableScope::new(scope.clone()),
                if memo {
                    Memo::new()
                } else {
                    Memo::empty()
                },
            ).into();

            *scope = scope.with_var(
                &name,
                fun.clone()
            );

            // Yeah I know, memory leaks, but overhauling
            // the runtime to use a GC that can tell the
            // difference between reachable and unreachable
            // values is more work than it's currently worth
            // TODO: fix this memory leak
            if let Value::Func(
                _,
                _,
                ref m_scope,
                _
            ) = *fun {
                m_scope.set(scope.clone());
            }

            Rc::new(Value::List(vec![]))
        },
        DefineRecBinary(name, arg_0, arg_1, var, memo) => {
            let fun: Rc<_> = Value::Func(
                Pattern::Binary(
                    ArgPattern::from_expr(&arg_0).unwrap(),
                    ArgPattern::from_expr(&arg_1).unwrap(),
                ),
                Rc::new(var),
                MutableScope::new(scope.clone()),
                if memo {
                    Memo::new()
                } else {
                    Memo::empty()
                },
            ).into();

            *scope = scope.with_var(
                &name,
                fun.clone()
            );

            if let Value::Func(
                _,
                _,
                ref m_scope,
                _,
            ) = *fun {
                m_scope.set(scope.clone());
            }

            Rc::new(Value::List(vec![]))
        },
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
                    Expr::Macro(ref name, ref args) => mutate_scope_with_action(
                        eval_macro(name, args.clone()),
                        &mut inner_scope,
                    ),
                    ref any => eval(any, inner_scope.clone()),
                };
            }

            out
        },
        Expr::UnaryFnCall(ref exp, ref arg) => {
            let param = Args::Unary(eval(arg, variables.clone()));

            let mut scope = variables;

            loop {
                let func = eval(exp, scope.clone());

                match *func {
                    Value::Func(ref pat, ref exp, ref captured, ref memo) =>
                        if let Some(val) = eval_fn(
                            (pat, exp, captured, memo),
                            param.clone()
                        ) {
                            return val;
                        } else {
                            scope = scope.parent().expect(
                                "Pattern not matched"
                            );
                        },
                    Value::BuiltinFunc(ref f) => return f(param),
                    _ => scope = scope.parent().expect(
                        "Pattern not matched"
                    ),
                }
            }
        },
        Expr::BinaryFnCall(ref exp, ref arg_0, ref arg_1) => {
            let params = Args::Binary(
                eval(arg_0, variables.clone()),
                eval(arg_1, variables.clone()),
            );

            let mut scope = variables;

            // TODO: This is super slow on a pattern miss, but in other
            //       situations is as fast as normal (+- compiler
            //       optimisations). How would I optimise this if I needed to?
            // XXX: Number one would be continue if the function is the same --
            //      would it be necessary to memoise patterns if I did this?
            //      It's certainly simpler than adding memoisation to Rust-side
            //      functions.
            loop {
                let func = eval(exp, scope.clone());

                match *func {
                    Value::Func(ref pat, ref exp, ref captured, ref memo) =>
                        if let Some(val) = eval_fn(
                            (pat, exp, captured, memo),
                            params.clone()
                        ) {
                            return val;
                        } else {
                            scope = scope.parent().expect(
                                "Pattern not matched"
                            );
                        },
                    Value::BuiltinFunc(ref f) =>
                        return f(params.clone()),
                    _ => scope = scope.parent().expect(
                        "Pattern not matched"
                    ),
                }
            }
        },
        Expr::If(ref cond, ref a, ref b) => {
            let c = eval(cond, variables.clone());

            if *c == val_false!() {
                eval(b, variables)
            } else {
                eval(a, variables)
            }
        },
        Expr::Func(ref pat, ref body, ref memo) =>
            Value::Func(
                pat.clone(),
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
            fn $name(args: Args) -> Rc<Value> {
                if let Args::Unary(val) = args {
                    $(
                        if let $pattern = *val {
                            return val_true!().into();
                        }
                    )+

                    val_false!().into()
                } else {
                    unimplemented!()
                }
            }
        }
    }

    make_is_fn!(is_str, Str(_));
    make_is_fn!(is_list, List(_));
    make_is_fn!(is_int, Int(_));
    make_is_fn!(is_hash, Hash(_));
    make_is_fn!(
        is_fn,
        Func(..) | BuiltinFunc(..)
    );

    fn print(args: Args) -> Rc<Value> {
        if let Args::Unary(val) = args {
            if let Str(ref s) = *val {
                println!("{}", s)
            } else {
                println!("{}", val);
            }

            List(vec![]).into()
        } else {
            unimplemented!()
        }
    }

    fn inner(args: Args) -> Rc<Value> {
        if let Args::Unary(mutable) = args {
            match mutable.as_ref() {
                &Mutable(ref inner) => inner.borrow().clone(),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn make_mutable(args: Args) -> Rc<Value> {
        use std::cell::RefCell;

        if let Args::Unary(a) = args {
            Mutable(RefCell::new(a)).into()
        } else {
            unimplemented!()
        }
    }

    fn mutate(args: Args) -> Rc<Value> {
        if let Args::Binary(a, new_val) = args {
            match a.as_ref() {
                &Mutable(ref inner) => {
                    *inner.borrow_mut() = new_val;

                    List(vec![]).into()
                },
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn add(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            match (a.as_ref(), b.as_ref()) {
                (&Int(ref i_a), &Int(ref i_b)) => Int(i_a + i_b).into(),
                _ => panic!("Cannot add {:?} and {:?}", a, b),
            }
        } else {
            unimplemented!()
        }
    }

    fn modulo(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            match (a.as_ref(), b.as_ref()) {
                (&Int(ref i_a), &Int(ref i_b)) => Int(i_a % i_b).into(),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn mul(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            match (a.as_ref(), b.as_ref()) {
                (&Int(ref i_a), &Int(ref i_b)) => Int(i_a * i_b).into(),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn div(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            match (a.as_ref(), b.as_ref()) {
                (&Int(ref i_a), &Int(ref i_b)) => Int(i_a / i_b).into(),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    fn cons(args: Args) -> Rc<Value> {
        if let Args::Binary(a_rc, b_rc) = args {
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
        } else {
            unimplemented!()
        }
    }

    fn eq(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            if a == b {
                val_true!().into()
            } else {
                val_false!().into()
            }
        } else {
            unimplemented!()
        }
    }

    fn less_or_eq(args: Args) -> Rc<Value> {
        if let Args::Binary(a, b) = args {
            if let (&Int(ref a_i), &Int(ref b_i)) = (a.as_ref(), b.as_ref()) {
                if a_i <= b_i {
                    val_true!().into()
                } else {
                    val_false!().into()
                }
            } else {
                val_false!().into()
            }
        } else {
            unimplemented!()
        }
    }

    fn neg(args: Args) -> Rc<Value> {
        if let Args::Unary(a) = args {
            match a.as_ref() {
                &Int(ref i_a) => Int(-i_a).into(),
                _ => unimplemented!(),
            }
        } else {
            unimplemented!()
        }
    }

    Scope::new_with_var("print", Value::BuiltinFunc(print).into())
        .with_var("=", Value::BuiltinFunc(eq).into())
        .with_var("<=", Value::BuiltinFunc(less_or_eq).into())
        .with_var("+", Value::BuiltinFunc(add).into())
        .with_var("*", Value::BuiltinFunc(mul).into())
        .with_var("/", Value::BuiltinFunc(div).into())
        .with_var("%", Value::BuiltinFunc(modulo).into())
        .with_var("cons", Value::BuiltinFunc(cons).into())
        .with_var("set!", Value::BuiltinFunc(mutate).into())
        .with_var("-", Value::BuiltinFunc(neg).into())
        .with_var("!", Value::BuiltinFunc(inner).into())
        .with_var("mut.", Value::BuiltinFunc(make_mutable).into())
        .with_var("str?", Value::BuiltinFunc(is_str).into())
        .with_var("list?", Value::BuiltinFunc(is_list).into())
        .with_var("int?", Value::BuiltinFunc(is_int).into())
        .with_var("hash?", Value::BuiltinFunc(is_hash).into())
        .with_var("fn?", Value::BuiltinFunc(is_fn).into())
}
