#[macro_use] extern crate nom;

mod ast;
mod parser;
mod error;
mod compiler;
mod source;

use std::env;
use std::process::exit;

use error::Error;
use compiler::Compiler;
use source::Source;

fn main() {
    // Note: Skip first argument (program name)
    let read = match env::args().nth(1) {
        Some(arg) => Source::from_file(arg),
        None => Source::from_stdin(),
    };

    let source = match read {
        Ok(s) => s,
        Err(e) => {
            println!("Error on reading source: {}", e);
            exit(4)
        },
    };

    let compiler = Compiler {};
    let ret = match compiler.parse(&source) {
        Ok(ast) => {
            println!("AST: {:?}", ast);
            0
        },
        Err(Error::OnParse{msg}) => {
            println!("{}", msg);
            5
        },
    };
    exit(ret);
}
