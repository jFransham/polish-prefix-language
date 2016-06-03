#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(question_mark)]

extern crate combine;

use combine::*;
use combine::primitives::Stream;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::hash::{SipHasher, Hash, Hasher};
use std::cell::RefCell;

macro_rules! hash {
    ($e:expr) => {{
        let mut hasher = SipHasher::new();

        ($e).hash(&mut hasher);

        hasher.finish()
    }};
}

macro_rules! val_true { () => { Value::Hash(hash!("t")) }; }
macro_rules! val_false { () => { Value::Hash(hash!("f")) }; }

#[derive(Debug, Clone)]
enum Value {
    Int(isize),
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
        use Value::*;

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
        use Value::*;

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
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        use Value::*;

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

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
enum Expr {
    UnaryFnCall(Box<Expr>, Box<Expr>),
    BinaryFnCall(Box<Expr>, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    UnaryFn(String, Box<Expr>),
    BinaryFn(String, String, Box<Expr>),
    Lit(Value),
    Variable(String),
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Macro(String, Vec<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter) -> Result<(), std::fmt::Error> {
        use Expr::*;

        match *self {
            UnaryFnCall(ref func, ref arg) =>
                write!(f, "(({}):{})", func, arg),
            BinaryFnCall(ref func, ref arg_0, ref arg_1) =>
                write!(f, "(({}): {} {})", func, arg_0, arg_1),
            UnaryFn(ref arg, ref expr) =>
                write!(f, "($fn {} {})", arg, expr),
            BinaryFn(ref arg_0, ref arg_1, ref expr) =>
                write!(f, "($fn {} {} {})", arg_0, arg_1, expr),
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

enum Action {
    DefineVariable(String, Expr),
    DefineRecUnary(String, String, Expr),
    DefineRecBinary(String, String, String, Expr),
    Eval(Expr),
}

fn escape_char(c: char) -> char {
    match c {
        '\'' => '\'',
        '"' => '"',
        '\\' => '\\',
        '/' => '/',
        'b' => '\u{0008}',
        'f' => '\u{000c}',
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        c => c,
    }
}

fn identifier<I: Stream<Item=char>>(input: State<I>) -> ParseResult<String, I> {
    (
        letter(),
        many(
            choice([
                box alpha_num() as Box<Parser<Output=_, Input=_>>,
                box char('_') as Box<Parser<Output=_, Input=_>>
            ])
        ),
    ).map(
        |(a, v): (_, Vec<_>)|
            Some(a).into_iter().chain(v.into_iter()).collect()
    ).parse_lazy(input)
}

fn macro_call<I: Stream<Item=char>>(input: State<I>) -> ParseResult<Expr, I> {
    (
        char('$'),
        parser(identifier),
        many(
            try(
                ((space(), spaces()), parser(expr))
            )
        )
    ).map(
        |(_, name, elements): (_, _, Vec<_>)| Expr::Macro(
            name,
            elements.into_iter().map(|(_, t)| t).collect()
        )
    ).parse_lazy(input)
}

fn string_literal<I: Stream<Item=char>>(
    input: State<I>
) -> ParseResult<String, I> {
    between(
        string("\""),
        string("\""),
        many(parser(string_char))
    ).parse_lazy(input)
}

fn string_char<I: Stream<Item=char>>(input: State<I>) -> ParseResult<char, I> {
    let (c, input) = try!(any().parse_lazy(input));
    let mut backslash_char = satisfy(
        |c| "\"\\/bfnrt".chars().any(|x| x == c)
    ).map(escape_char);

    match c {
        '\\' => input.combine(|input| backslash_char.parse_state(input)),
        '"' => unexpected("\"").parse_state(
            input.into_inner()
        ).map(|_| unreachable!()),
        _ => Ok((c, input)),
    }
}

fn unit<I: Stream<Item=char>>(input: State<I>) -> ParseResult<Expr, I> {
    use std::str::FromStr;

    let int = many1(digit()).map(
        |d: String| Expr::Lit(
            Value::Int(
                isize::from_str(&d).unwrap()
            )
        )
    );

    let hash = (char::<I>('#'), parser(identifier)).map(
        |(_, i): (_, String)| Value::Hash(hash!(i))
    ).map(Expr::Lit);

    let variable = parser(identifier).map(Expr::Variable);

    let fn_variable = try(
        between(
            (char('('), spaces()),
            (spaces(), char(')')),
            parser(fn_name)
        )
    );

    let block = between(
        (char('('), spaces()),
        char(')'),
        parser(expr_list),
    );

    let list = between(
        (char('['), spaces()),
        (spaces(), char(']')),
        sep_by(
            parser(expr),
            (space(), spaces()),
        ),
    ).map(Expr::List);

    choice([
        box hash as Box<Parser<Output=_, Input=_>>,
        box fn_variable as Box<Parser<Output=_, Input=_>>,
        box block as Box<Parser<Output=_, Input=_>>,
        box list as Box<Parser<Output=_, Input=_>>,
        box int as Box<Parser<Output=_, Input=_>>,
        box variable as Box<Parser<Output=_, Input=_>>,
        box parser(string_literal)
            .map(Value::Str)
            .map(Expr::Lit) as Box<Parser<Output=_, Input=_>>,
    ]).parse_lazy(input)
}

fn fn_name<I: Stream<Item=char>>(input: State<I>) -> ParseResult<Expr, I> {
    choice([
        box char('\\').map(
            |e: char| Expr::Variable(e.to_string())
        ) as Box<Parser<Output=_, Input=_>>,
        box try(
            (
                optional(parser(identifier)),
                many1(
                    choice([
                        char('+'),
                        char('-'),
                        char('/'),
                        char('*'),
                        char('&'),
                        char('|'),
                        char('~'),
                        char('!'),
                        char('^'),
                        char(','),
                        char('.'),
                        char('%'),
                        char('='),
                        char('<'),
                        char('>'),
                        char('@'),
                        char('`'),
                        char('?'),
                    ])
                ),
            )
        ).map(|(a, b): (Option<String>, String)| {
            if let Some(mut s) = a {
                s.push_str(&b);

                Expr::Variable(s)
            } else {
                Expr::Variable(b)
            }
        }) as Box<Parser<Output=_, Input=_>>,
        box try(
            (
                parser(unit),
                char(':'),
            ).map(
                |(fun, _)| fun
            )
        ) as Box<Parser<Output=_, Input=_>>,
    ]).parse_lazy(input)
}

fn expr<I: Stream<Item=char>>(input: State<I>) -> ParseResult<Expr, I> {
    let binary_func_call = (
        parser(fn_name),
        (space(), spaces()),
        parser(expr),
        (space(), spaces()),
        parser(expr)
    ).map(
        |(fun, _, left, _, right)|
            Expr::BinaryFnCall(box fun, box left, box right)
    );

    let unary_func_call = (
        parser(fn_name),
        parser(expr),
    ).map(|(fun, param)| Expr::UnaryFnCall(box fun, box param));

    choice([
        box parser(macro_call) as Box<Parser<Output=_, Input=_>>,
        box try(binary_func_call) as Box<Parser<Output=_, Input=_>>,
        box try(unary_func_call) as Box<Parser<Output=_, Input=_>>,
        box parser(unit) as Box<Parser<Output=_, Input=_>>,
    ]).parse_lazy(input)
}

fn expr_list<I: Stream<Item=char>>(
    input: State<I>
) -> ParseResult<Expr, I> {
    (
        spaces(),
        sep_by(
            (parser(expr), spaces()),
            (char(';'), spaces()),
        )
    ).map(
        |(_, v): (_, Vec<_>)| v.into_iter().map(|(e, _)| e).collect()
    ).map(
        Expr::Block
    ).parse_lazy(input)
}

fn eval_macro(macro_name: &str, args: Vec<Expr>) -> Action {
    use Action::*;

    match macro_name {
        "def" => {
            use Expr::*;

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
                        val
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
                        val
                    ),
                (
                    Some(Variable(n)),
                    Some(val),
                    None,
                ) =>
                    DefineVariable(n, val),
                _ => unimplemented!(),
            }
        },
        "fn" => {
            use Expr::*;

            let mut iter = args.into_iter();

            match (iter.next(), iter.next(), iter.next(), iter.next()) {
                (Some(Variable(arg)), Some(val), None, None) =>
                    Eval(UnaryFn(arg, box val)),
                (
                    Some(Variable(arg_0)),
                    Some(Variable(arg_1)),
                    Some(val),
                    None
                ) =>
                    Eval(BinaryFn(arg_0, arg_1, box val)),
                _ => unimplemented!(),
            }
        },
        "if" => {
            use Expr::*;

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

fn eval(expression: &Expr, variables: Scope) -> Rc<Value> {
    use Action::*;

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
                            DefineRecUnary(name, arg, var) => {
                                let fun: Rc<_> = Value::UnaryFn(
                                    arg,
                                    Rc::new(var),
                                    MutableScope::new(inner_scope.clone()),
                                    Memo::new(),
                                ).into();

                                inner_scope = inner_scope.with_var(
                                    &name,
                                    fun.clone()
                                );

                                if let Value::UnaryFn(
                                    _,
                                    _,
                                    ref cell,
                                    _
                                ) = *fun {
                                    *cell.0.borrow_mut() = inner_scope.clone();
                                }

                                Rc::new(Value::List(vec![]))
                            },
                            DefineRecBinary(name, arg_0, arg_1, var) => {
                                let fun: Rc<_> = Value::BinaryFn(
                                    arg_0,
                                    arg_1,
                                    Rc::new(var),
                                    MutableScope::new(inner_scope.clone()),
                                    Memo::new(),
                                ).into();

                                inner_scope = inner_scope.with_var(
                                    &name,
                                    fun.clone()
                                );

                                if let Value::BinaryFn(
                                    _,
                                    _,
                                    _,
                                    ref cell,
                                    _,
                                ) = *fun {
                                    *cell.0.borrow_mut() = inner_scope.clone();
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
                        let inner_scope = captured.0.borrow().with_var(
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
                        let inner_scope = captured.0.borrow()
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
        Expr::UnaryFn(ref param_name, ref body) =>
            Value::UnaryFn(
                param_name.clone(),
                Rc::new(*body.clone()),
                MutableScope::new(variables.clone()),
                Memo::new(),
            ).into(),
        Expr::BinaryFn(ref param_name_0, ref param_name_1, ref body) =>
            Value::BinaryFn(
                param_name_0.clone(),
                param_name_1.clone(),
                Rc::new(*body.clone()),
                MutableScope::new(variables.clone()),
                Memo::new(),
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
fn stdlib() -> Scope {
    use Value::*;

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
        if let (&Int(a_i), &Int(b_i)) = (a.as_ref(), b.as_ref()) {
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

#[derive(Debug, Clone)]
struct Memo<T: Hash + Eq>(RefCell<HashMap<T, Rc<Value>>>);

type UnaryMemo = Memo<Rc<Value>>;
type BinaryMemo = Memo<(Rc<Value>, Rc<Value>)>;

impl<T: Hash + Eq> Memo<T> {
    fn new() -> Self {
        Memo(RefCell::new(HashMap::new()))
    }

    fn get(&self, value: &T) -> Option<Rc<Value>> {
        (*self.0.borrow()).get(value).cloned()
    }

    fn insert(&self, input: T, output: Rc<Value>) {
        (*self.0.borrow_mut()).insert(input, output);
    }
}

impl<T: Hash + Eq> Default for Memo<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
struct MutableScope(RefCell<Scope>);

impl MutableScope {
    fn new(scope: Scope) -> Self {
        MutableScope(RefCell::new(scope))
    }
}

impl Hash for MutableScope {
    fn hash<T: Hasher>(&self, hasher: &mut T) {
        (Rc::as_ref(&self.0.borrow().0) as *const _).hash(hasher)
    }
}

impl PartialEq for MutableScope {
    fn eq(&self, other: &Self) -> bool {
        *self.0.borrow() == *other.0.borrow()
    }
}

impl Eq for MutableScope { }

#[derive(Debug, Clone)]
struct Scope(Rc<OwnedScope>);

impl Scope {
    fn get(&self, name: u64) -> Option<Rc<Value>> {
        self.0.get(name)
    }

    fn new_with_var(name: &str, value: Rc<Value>) -> Scope {
        Rc::new(
            OwnedScope {
                parent: None,
                name: hash!(name),
                value: value,
            }
        ).into()
    }

    fn with_var(&self, name: &str, value: Rc<Value>) -> Scope {
        Rc::new(
            OwnedScope {
                parent: Some(self.clone()),
                name: hash!(name),
                value: value,
            }
        ).into()
    }
}

impl PartialEq for Scope {
    fn eq(&self, other: &Self) -> bool {
        (Rc::as_ref(&self.0) as *const _) == (Rc::as_ref(&other.0) as *const _)
    }
}

impl Eq for Scope { }

impl From<Rc<OwnedScope>> for Scope {
    fn from(scope: Rc<OwnedScope>) -> Self {
        Scope(scope)
    }
}

#[derive(Debug, Clone)]
struct OwnedScope {
    parent: Option<Scope>,
    name: u64,
    value: Rc<Value>,
}

impl OwnedScope {
    fn get(&self, key: u64) -> Option<Rc<Value>> {
        if self.name == key {
            Some(self.value.clone())
        } else {
            if let Some(ref p) = self.parent {
                p.get(key)
            } else {
                None
            }
        }
    }
}

fn main() {
    use std::io::prelude::*;
    use std::fs::File;

    let mut f = File::open("test.pfx").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let program = parser(expr_list).parse(
        &s as &str
    ).expect("Syntax error");

    if !program.1.is_empty() {
        println!("Syntax error (whole file not consumed)");
        println!("Rest:");
        panic!("{}", program.1);
    }

    println!(
        "\nOutput: {}",
        eval(
            &program.0,
            stdlib()
        )
    );
}
