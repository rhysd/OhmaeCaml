#[macro_use] extern crate nom;

mod ast;
mod parser;
mod error;

use std::env;
use std::io;
use std::fs;
use std::io::Read;

use parser::parse;
use error::Error;

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
    // let code = read_source(env::args().collect::<Vec<_>>()).expect("Error on reading source code");
    match parse("1.1e2+10") {
        Ok(ast) => println!("AST: {:?}", ast),
        Err(Error::OnParse{msg}) => println!("{}", msg),
    }
}
