#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate combine;

use combine::*;
use combine::primitives::Stream;
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Value {
    Int(isize),
    Str(String),
    UnaryFn(String, Box<Expr>), // Eventually: closure
    BinaryFn(String, String, Box<Expr>), // Eventually: closure
    BuiltinUnaryFn(fn(Value) -> Value),
    BuiltinBinaryFn(fn(Value, Value) -> Value),
    List(Vec<Value>),
}

#[derive(Debug, Clone)]
enum Expr {
    UnaryFnCall(Box<Expr>, Box<Expr>),
    BinaryFnCall(Box<Expr>, Box<Expr>, Box<Expr>),
    UnaryFn(String, Box<Expr>),
    BinaryFn(String, String, Box<Expr>),
    Lit(Value),
    Variable(String),
    List(Vec<Expr>),
    Block(Vec<Expr>),
    Macro(String, Vec<Expr>),
}

enum Action {
    DefineVariable(String, Expr),
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
        |(a, v): (_, Vec<_>)| Some(a).into_iter().chain(v.into_iter()).collect()
    ).parse_lazy(input)
}

fn macro_call<I: Stream<Item=char>>(input: State<I>) -> ParseResult<Expr, I> {
    (
        char('$'),
        parser(identifier),
        many(
            (try(spaces()), parser(expr))
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
        (spaces(), char(')')),
        parser(expr_list),
    );

    let forced_new_expr = (char('\\'), parser(expr)).map(|(_, e)| e);

    let list = between(
        (char('['), spaces()),
        (spaces(), char(']')),
        sep_by(
            parser(expr),
            (space(), spaces()),
        ),
    ).map(Expr::List);

    choice([
        box forced_new_expr as Box<Parser<Output=_, Input=_>>,
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
        box many1(
            choice([
                char('+'),
                char('-'),
                char('/'),
                char('*'),
                char('&'),
                char('%'),
                char('#'),
            ])
        ).map(Expr::Variable) as Box<Parser<Output=_, Input=_>>,
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

fn eval_macro(macro_name: String, args: Vec<Expr>) -> Action {
    use Action::*;

    match (&macro_name) as &str {
        "def" => {
            use Expr::*;

            let mut iter = args.into_iter();

            match (iter.next(), iter.next(), iter.next()) {
                (
                    Some(UnaryFnCall(box Variable(n), box Variable(arg))),
                    Some(val),
                    None,
                ) =>
                    DefineVariable(n, UnaryFn(arg, box val)),
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
                    DefineVariable(n, BinaryFn(arg_0, arg_1, box val)),
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
        _ => unimplemented!(),
    }
}

fn eval(expression: Expr, variables: &mut Vec<HashMap<String, Value>>) -> Value {
    use Action::*;

    variables.push(HashMap::new());

    let out = match expression {
        Expr::Lit(v) => v,
        Expr::Macro(name, args) => match eval_macro(name, args) {
            Eval(e) => eval(e, variables),
            _       => unimplemented!(),
        },
        Expr::Block(exprs) => {
            let mut out = Value::List(vec![]);

            for ex in exprs {
                out = match ex {
                    Expr::Macro(name, args) => match eval_macro(name, args) {
                        Eval(e) => eval(e, variables),
                        DefineVariable(name, var) => {
                            let val = eval(var, variables);

                            if let Some(h) = variables.last_mut() {
                                h.insert(name, val);
                            }

                            Value::List(vec![])
                        },
                    },
                    any => eval(any, variables),
                };
            }

            out
        },
        Expr::UnaryFnCall(box exp, box arg) => {
            let func = eval(exp, variables);
            let parameter = eval(arg, variables);

            match func {
                Value::UnaryFn(param_name, box exp) => {
                    let mut args = HashMap::new();

                    args.insert(param_name, parameter);

                    variables.push(args);

                    let out = eval(exp, variables);

                    variables.pop();

                    out
                },
                Value::BuiltinUnaryFn(f) => f(parameter),
                _ => unimplemented!(),
            }
        },
        Expr::BinaryFnCall(box exp, box arg_0, box arg_1) => {
            let func = eval(exp, variables);
            let params = (
                eval(arg_0, variables),
                eval(arg_1, variables),
            );

            match func {
                Value::BinaryFn(param_name_0, param_name_1, box exp) => {
                    let mut args = HashMap::new();

                    args.insert(param_name_0, params.0);
                    args.insert(param_name_1, params.1);

                    variables.push(args);

                    let out = eval(exp, variables);

                    variables.pop();

                    out
                },
                Value::BuiltinBinaryFn(f) => f(params.0, params.1),
                _ => unimplemented!(),
            }
        },
        Expr::UnaryFn(param_name, body) => Value::UnaryFn(param_name, body),
        Expr::BinaryFn(param_name_0, param_name_1, body) =>
            Value::BinaryFn(param_name_0, param_name_1, body),
        Expr::Variable(name) => variables.iter().rev().filter_map(
            |h| h.get(&name)
        ).next().expect(&format!("Cannot find variable {}", name)).clone(),
        Expr::List(exprs) => Value::List(
            exprs.into_iter().map(
                |e| eval(e, variables)
            ).collect()
        ),
    };

    variables.pop();

    out
}

fn stdlib() -> HashMap<String, Value> {
    use Value::*;

    fn print(val: Value) -> Value {
        match val {
            Str(s) => println!("{}", s),
            Int(i) => println!("{}", i),
            List(list) => println!("{:?}", list),
            any => println!("{:?}", any),
        }

        List(vec![])
    }

    fn add(a: Value, b: Value) -> Value {
        match (a, b) {
            (Int(i_a), Int(i_b)) => Int(i_a + i_b),
            _ => unimplemented!(),
        }
    }

    fn neg(a: Value) -> Value {
        match a {
            Int(i_a) => Int(-i_a),
            _ => unimplemented!(),
        }
    }

    let mut out = HashMap::new();

    out.insert("print".into(), Value::BuiltinUnaryFn(print));
    out.insert("+".into(), Value::BuiltinBinaryFn(add));
    out.insert("-".into(), Value::BuiltinUnaryFn(neg));

    out
}

fn main() {
    use std::io::prelude::*;
    use std::fs::File;

    let mut f = File::open("test.pfx").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let program = parser(expr_list).parse(
        &s as &str
    ).expect("Syntax error").0;

    println!("{:#?}", program);

    println!(
        "{:?}",
        eval(
            program,
            &mut vec![stdlib()]
        )
    );
}
