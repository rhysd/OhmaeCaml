#[macro_use] extern crate nom;

mod ast;
mod parser;
mod error;
mod compiler;

use std::env;
use std::io;
use std::fs;
use std::io::Read;
use std::process::exit;

use error::Error;
use compiler::Compiler;

fn read_source(argv: Vec<String>) -> io::Result<String> {
    let mut buf = String::new();
    if argv.len() > 1 {
        let mut f = fs::File::open(&argv[1])?;
        f.read_to_string(&mut buf)?;
    } else {
        io::stdin().read_to_string(&mut buf)?;
    }
    Ok(buf)
}

fn main() {
    let code = read_source(env::args().collect::<Vec<_>>()).expect("Error on reading source code");
    let compiler = Compiler::new(code.as_str());
    let code = match compiler.parse() {
        Ok(ast) => {
            println!("AST: {:?}", ast);
            0
        },
        Err(Error::OnParse{msg}) => {
            println!("{}", msg);
            4
        },
    };
    exit(code);
}
