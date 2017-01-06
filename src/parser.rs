use std::str;
use std::str::FromStr;

use nom::{
    IResult,
    newline,
    digit,
    alpha,
    alphanumeric,
    Needed,
};

use ast::*;
use error::Error;
use source::Source;

fn offset(input: &[u8]) -> IResult<&[u8], usize> {
    IResult::Done(input, input.len())
}

pub struct Parser<'a> {
    source: &'a Source,
}

named!(integer<ConstValue>, map!(
    map_res!(
        map_res!(
            digit,
            str::from_utf8
        ),
        FromStr::from_str
    ),
    ConstValue::Int)
);

named!(unit<ConstValue>, do_parse!(
    ws!(pair!(char!('('), char!(')'))) >>
    (ConstValue::Unit)
));

named!(boolean<ConstValue>, do_parse!(
    b: alt!(tag!(b"true") | tag!(b"false")) >>
    not!(peek!(alphanumeric)) >>
    (ConstValue::Bool(true))
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

named!(float<ConstValue>,
    map!(
        map_res!(
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
        ),
        ConstValue::Float
    )
);

named!(identifier<String>,
    map!(
        map_res!(
            recognize!(
                pair!(alpha, many0!(alphanumeric))
            ),
            str::from_utf8
        ),
        String::from
    )
);

named!(var<Var>,
    map!(
        pair!(offset, identifier),
        |(o, i)| Var {name: i, offset: o}
    )
);

named!(constant<Constant>, do_parse!(
    o: offset >>
    v: alt!(unit | boolean | float | integer) >>
    (Constant { value: v, offset: o })
));

/* TODO
named!(get<Get>, ws!(do_parse!(
    o: offset >>
    l: paren_less_expr >>
    char!('.') >>
    char!('(') >> 
    i: expr >>
    char!(')') >>
    (Get {array: Box::new(l), i: Box::new(i), offset: o})
)));
*/

named!(paren_less_expr<Expr>,
    alt!(
        /* TODO: delimited!(char!('('), expr , char!(')')) */
        map!(constant, Expr::Constant) |
        map!(var, Expr::Var)
        /* TODO: map!(get, Expr::Get) */
    )
);

pub fn parse<S: AsRef<str>>(code: S) -> Result<AST, Error> {
    match paren_less_expr(code.as_ref().as_bytes()) {
        IResult::Done(_, p) => Ok(AST {root: p}),
        IResult::Error(err) => Err(Error::OnParse { msg: format!("Parse error: {}", err) }),
        IResult::Incomplete(Needed::Size(n)) => Err(Error::OnParse { msg: format!("Parsed incomplete input: More {} bytes are needed", n) }),
        IResult::Incomplete(Needed::Unknown) => Err(Error::OnParse { msg: "Parsed incomplete input".to_string() }),
    }
}
