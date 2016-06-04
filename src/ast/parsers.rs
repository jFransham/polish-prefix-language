use combine::*;
use combine::primitives::Stream;
use num_bigint::BigInt;

use ast::expr::*;
use eval::Value;

macro_rules! hash {
    ($e:expr) => {{
        use std::hash::{SipHasher, Hash, Hasher};

        let mut hasher = SipHasher::new();

        ($e).hash(&mut hasher);

        hasher.finish()
    }};
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
                BigInt::from_str(&d).unwrap()
            )
        )
    );

    let hash = (
        char::<I>('#'),
        parser(identifier)
    ).map(
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

pub fn entry_point<I: Stream<Item=char>>(
    stream: I
) -> Result<(Expr, I), ParseError<I>> {
    parser(expr_list).parse(stream)
}
