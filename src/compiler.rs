use ast::Program;
use parser::parse;
use error::Error;

#[derive(Debug)]
pub struct Compiler<'a> {
    pub code: &'a str,
}

impl<'a> Compiler<'a> {
    pub fn new<'code>(c: &'code str) -> Compiler<'code> {
        Compiler { code: c }
    }

    pub fn parse(&self) -> Result<Program, Error> {
        parse(self.code)
    }
}
