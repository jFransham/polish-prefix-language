#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(stmt_expr_attributes)]
#![feature(question_mark)]

extern crate combine;
extern crate num_bigint;

mod ast;
mod eval;

use ast::*;
use eval::*;

fn main() {
    use std::io::prelude::*;
    use std::fs::File;

    let mut f = File::open("test.pfx").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s).unwrap();

    let program = parse(&s as &str).expect("Syntax error");

    println!(
        "\nOutput: {}",
        eval(
            &program,
            stdlib()
        )
    );
}
