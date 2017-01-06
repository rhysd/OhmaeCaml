use ast::AST;
use source::Source;
use parser::parse;
use error::Error;

#[derive(Debug)]
pub struct Compiler {
    // Options such as optimization level will be here
}

impl Compiler {
    pub fn parse(&self, source: &Source) -> Result<AST, Error> {
        parse(source.code())
    }
}
