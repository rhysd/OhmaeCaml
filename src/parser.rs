use nom::{
    IResult,
    newline,
    multispace,
    be_f64,
};

use ast::*;
use error::Error;

named!(program<Program>,
    map!(
        complete!(
            separated_list!(newline, ws!(add))
        ),
        |es| Program { body: es }
    )
);

named!(add<Expr>,
    alt!(
        do_parse!(
            lhs: mul >>
            opt!(multispace) >>
            op: alt!(char!('+') | char!('-')) >>
            opt!(multispace) >>
            rhs: mul >> (
                Expr::BinOpExpr(
                    BinOpExpr {op: bin_op_from_char(op), lhs: Box::new(lhs), rhs: Box::new(rhs)}
                )
            )
        ) | mul
    )
);

named!(mul<Expr>,
    alt!(
        do_parse!(
            lhs: fact >>
            opt!(multispace) >>
            op: alt!(char!('*') | char!('/')) >>
            opt!(multispace) >>
            rhs: fact >> (
                Expr::BinOpExpr(
                    BinOpExpr {op: bin_op_from_char(op), lhs: Box::new(lhs), rhs: Box::new(rhs)}
                )
            )
        ) | fact
    )
);

named!(fact<Expr>,
    alt!(
        map!(term, |t| Expr::Constant(t)) |
        ws!(delimited!(char!('('), add, char!(')')))
    )
);

named!(term<Constant>,
    map!(be_f64, |v| Constant::Num(v))
);

pub fn parse<S: AsRef<str>>(input: S) -> Result<Program, Error> {
    match program(input.as_ref().as_bytes()) {
        IResult::Done(_, e) => Ok(e),
        IResult::Error(err) => Err(Error::OnParse { msg: format!("Parse error: {}", err) }),
        IResult::Incomplete(n) => panic!("Parse result must not be incomplete: {:?}", n),
    }
}
