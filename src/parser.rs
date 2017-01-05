use std::str;
use std::str::FromStr;

use nom::{
    IResult,
    newline,
    digit,
    Needed,
};

use ast::*;
use error::Error;

fn last_offset(input: &[u8]) -> IResult<&[u8], usize> {
    IResult::Done(input, input.len())
}

fn bin_op_from_char(ch: char) -> BinOp {
    match ch {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        _   => panic!("Invalid binary operator {}", ch),
    }
}

fn unary_op_from_char(ch: char) -> UnaryOp {
    match ch {
        '-' => UnaryOp::Neg,
        '+' => UnaryOp::Pos,
        _ => panic!("Invalid unary operator {}", ch),
    }
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
        |acc, (op, rhs) | Expr::BinOpExpr(
            BinOpExpr {
                op: bin_op_from_char(op),
                lhs: Box::new(acc),
                rhs: Box::new(rhs)
            }
        )
    ) >>
    (folded)
));

named!(unary<Expr>, map!(
    ws!(
        pair!(
            alt!(char!('+') | char!('-')),
            term
        )
    ),
    |(op, c)| Expr::UnaryOpExpr(
        UnaryOpExpr {
            op: unary_op_from_char(op),
            child: Box::new(c),
        }
    )
));

named!(fact<Expr>, alt!(unary | term));

named!(term<Expr>,
    alt!(
        ws!(delimited!(char!('('), add, char!(')'))) |
        map!(constant, Expr::Constant)
    )
);

named!(constant<Constant>, do_parse!(
    o: last_offset >>
    v: float >>
    (Constant::Num(Num {
        value: v,
        offset: o,
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
