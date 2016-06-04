use combine::primitives::{Error, Info, Positioner};
use combine::ParseError;

mod expr;
mod parsers;

pub use self::expr::Expr;

pub fn parse(stream: &str) -> Result<Expr, ParseError<&str>> {
    parsers::entry_point(stream).and_then(
        |(out, rest)| if rest.is_empty() {
            Ok(out)
        } else {
            Err(
                ParseError::from_errors(
                    char::start(),
                    vec![Error::Unexpected(Info::Owned(rest.into()))]
                )
            )
        }
    )
}
