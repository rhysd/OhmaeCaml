use std::str;
use std::str::FromStr;

use nom::{
    IResult,
    newline,
    multispace,
    digit,
    Needed,
};

use ast::*;
use error::Error;

fn rest_input_len(input: &[u8]) -> IResult<&[u8], usize> {
    IResult::Done(input, input.len())
}

named!(program<Program>,
    map!(
        separated_list!(newline, ws!(add)),
        |es| Program { body: es }
    )
);

named!(add<Expr>, do_parse!(
    init: ws!(mul) >>
    folded: fold_many0!(
        pair!(alt!(char!('+') | char!('-')), ws!(mul)),
        init,
        |acc, (op, rhs)| Expr::BinOpExpr (
            BinOpExpr {
                op: bin_op_from_char(op),
                lhs: Box::new(acc),
                rhs: Box::new(rhs)
            }
        )
    ) >>
    (folded)
));

named!(mul<Expr>, do_parse!(
    init: ws!(fact) >>
    folded: fold_many0!(
        pair!(alt!(char!('*') | char!('/')), ws!(fact)),
        init,
        |acc, (op, rhs) | Expr::BinOpExpr (
            BinOpExpr {
                op: bin_op_from_char(op),
                lhs: Box::new(acc),
                rhs: Box::new(rhs)
            }
        )
    ) >>
    (folded)
));

named!(fact<Expr>,
    alt!(
        ws!(delimited!(char!('('), add, char!(')'))) |
        map!(term, Expr::Constant)
    )
);

named!(term<Constant>, do_parse!(
    r: rest_input_len >>
    v: float >>
    (Constant::Num(Num {
        value: v,
        rest: r,
    }))
));

named!(consume_frac<()>, do_parse!(
    char!('.') >> digit >> ()
));

named!(consume_exp<()>, do_parse!(
    alt!(char!('e') | char!('E')) >>
    opt!(alt!(char!('+') | char!('-'))) >>
    digit >>
    ()
));

named!(float<f64>, map_res!(
    map_res!(
        recognize!(
            delimited!(
                digit,
                opt!(complete!(consume_frac)),
                opt!(complete!(consume_exp))
            )
        ),
        str::from_utf8
    ),
    FromStr::from_str
));

pub fn parse<S: AsRef<str>>(input: S) -> Result<Program, Error> {
    match program(input.as_ref().as_bytes()) {
        IResult::Done(_, p) => Ok(p),
        IResult::Error(err) => Err(Error::OnParse { msg: format!("Parse error: {}", err) }),
        IResult::Incomplete(Needed::Size(n)) => Err(Error::OnParse { msg: format!("Parsed incomplete input: More {} bytes are needed", n) }),
        IResult::Incomplete(Needed::Unknown) => Err(Error::OnParse { msg: "Parsed incomplete input".to_string() }),
    }
}
