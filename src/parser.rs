use std::str;
use std::str::FromStr;

use nom::{
    IResult,
    digit,
    alpha,
    alphanumeric,
    multispace,
    Needed,
};

use ast::*;
use error::Error;

fn offset(input: &[u8]) -> IResult<&[u8], usize> {
    IResult::Done(input, input.len())
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

// TODO: Except for keywords
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

named!(var<Expr>,
    map!(
        pair!(offset, identifier),
        |(o, i)| Var::expr(i, o)
    )
);

named!(constant<Expr>, do_parse!(
    o: offset >>
    v: alt!(unit | boolean | float | integer) >>
    (Constant::expr(v, o))
));

named!(get<Expr>, ws!(do_parse!(
    o: offset >>
    array: paren_less_expr >>
    ws!(char!('.')) >>
    ws!(char!('(')) >> 
    index: expr >>
    ws!(char!(')')) >>
    (Get::expr(Box::new(array), Box::new(index), o))
)));

named!(paren_less_expr<Expr>,
    alt!(
        delimited!(char!('('), expr , char!(')')) |
        constant |
        var |
        get
    )
);

named!(logical_or<Expr>, do_parse!(
    init: logical_and >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >>
            o: offset >>
            tag!(b"||") >>
            opt!(multispace) >>
            rhs: logical_and >>
            ((o, rhs))
        ),
        init,
        |acc, (o, rhs)| BinOpExpr::expr(BinOp::Or, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(logical_and<Expr>, do_parse!(
    init: equal >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >> 
            o: offset >>
            tag!(b"&&") >>
            opt!(multispace) >>
            rhs: equal >>
            ((o, rhs))
        ),
        init,
        |acc, (o, rhs)| BinOpExpr::expr(BinOp::And, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(equal_op<BinOp>, alt!(
    tag!(b"==") => { |_| BinOp::Eq } |
    tag!(b"<>") => { |_| BinOp::NEq }
));

named!(equal<Expr>, do_parse!(
    init: relational >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >>
            o: offset >>
            op: equal_op >>
            opt!(multispace) >>
            rhs: relational >>
            ((o, op, rhs))
        ),
        init,
        |acc, (o, op, rhs)| BinOpExpr::expr(op, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(relational_op<BinOp>, alt!(
    tag!(b"<=") => { |_| BinOp::LessEq } |
    tag!(b">=") => { |_| BinOp::GreaterEq } |
    tag!(b"<") => { |_| BinOp::Less } |
    tag!(b">") => { |_| BinOp::Greater }
));

named!(relational<Expr>, do_parse!(
    init: additive >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >>
            o: offset >>
            op: relational_op >>
            opt!(multispace) >>
            rhs: additive >>
            ((o, op, rhs))
        ),
        init,
        |acc, (o, op, rhs)| BinOpExpr::expr(op, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(additive_op<BinOp>, alt!(
    tag!(b"+.") => { |_| BinOp::FAdd } |
    tag!(b"-.") => { |_| BinOp::FSub } |
    tag!(b"+") => { |_| BinOp::Add } |
    tag!(b"-") => { |_| BinOp::Sub }
));

named!(additive<Expr>, do_parse!(
    init: mult >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >>
            o: offset >>
            op: additive_op >>
            opt!(multispace) >>
            rhs: mult >>
            ((o, op, rhs))
        ),
        init,
        |acc, (o, op, rhs)| BinOpExpr::expr(op, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(mult_op<BinOp>, alt!(
    tag!(b"*.") => { |_| BinOp::FMul } |
    tag!(b"/.") => { |_| BinOp::FDiv } |
    tag!(b"*") => { |_| BinOp::Mul } |
    tag!(b"/") => { |_| BinOp::Div }
));

named!(mult<Expr>, do_parse!(
    init: unary >>
    folded: fold_many0!(
        do_parse!(
            opt!(multispace) >>
            o: offset >>
            op: mult_op >>
            opt!(multispace) >>
            rhs: unary >>
            ((o, op, rhs))
        ),
        init,
        |acc, (o, op, rhs)| BinOpExpr::expr(op, Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

named!(unary_op<UnaryOp>, alt!(
    tag!(b"-.") => { |_| UnaryOp::FNeg } |
    tag!(b"-") => { |_| UnaryOp::Neg } |
    tag!(b"!") => { |_| UnaryOp::Not }
));

named!(unary<Expr>, alt!(
    do_parse!(
        o: offset >>
        opt!(multispace) >>
        op: unary_op >>
        opt!(multispace) >>
        child: unary >>
        (UnaryOpExpr::expr(op, Box::new(child), o))
    ) |
    postfix
));

named!(put<Expr>, do_parse!(
    lhs: paren_less_expr >>
    o: offset >>
    ws!(char!('.')) >>
    ws!(char!('(')) >>
    index: expr >>
    ws!(char!(')')) >>
    ws!(tag!("<-")) >>
    rhs: expr >>
    (Put::expr(Box::new(lhs), Box::new(index), Box::new(rhs), o))
));

named!(apply<Expr>, do_parse!(
    o: offset >>
    callee: expr >>
    multispace >>
    args: separated_nonempty_list!(multispace, paren_less_expr) >>
    (Apply::expr(Box::new(callee), args, o))
));

named!(postfix<Expr>, alt!(
    put |
    apply
));

named!(array<Expr>, do_parse!(
    o: offset >>
    tag!("Array.create") >>
    multispace >>
    size: paren_less_expr >>
    multispace >>
    elem: paren_less_expr >>
    (Array::expr(Box::new(size), Box::new(elem), o))
));

named!(if_<Expr>, do_parse!(
    o: offset >>
    tag!("if") >>
    multispace >>
    cond: expr >>
    multispace >>
    tag!("then") >>
    multispace >>
    then: expr >>
    multispace >>
    tag!("else") >>
    expr >>
    otherwise: expr >>
    expr >>
    (If::expr(Box::new(cond), Box::new(then), Box::new(otherwise), o))
));

named!(let_tuple<Expr>, do_parse!(
    o: offset >>
    names: ws!(delimited!(char!('('), separated_nonempty_list!(char!(','), identifier), char!(')'))) >>
    opt!(multispace) >>
    char!('=') >>
    opt!(multispace) >>
    bound: expr >>
    multispace >>
    tag!("in") >>
    multispace >>
    body: expr >>
    (LetTuple::expr(names, Box::new(bound), Box::new(body), o))
));

named!(fundef<Fundef>, do_parse!(
    n: identifier >>
    p: many0!(do_parse!(multispace >> arg: identifier >> (arg))) >>
    multispace >>
    b: expr >>
    (Fundef { name: n, params: p, body: Box::new(b) })
));

named!(let_rec<Expr>, do_parse!(
    o: offset >>
    tag!("let") >>
    multispace >>
    tag!("rec") >>
    multispace >>
    fun: fundef >>
    multispace >>
    tag!("in") >>
    multispace >>
    body: expr >>
    (LetRec::expr(fun, Box::new(body), o))
));

named!(let_<Expr>, do_parse!(
    o: offset >>
    tag!("let") >>
    multispace >>
    name: identifier >>
    ws!(char!('=')) >>
    bound: expr >>
    multispace >>
    tag!("in") >>
    multispace >>
    body: expr >>
    (Let::expr(name, Box::new(bound), Box::new(body), o))
));

named!(tuple<Expr>,
    map!(
        pair!(
            offset,
            ws!(separated_nonempty_list!(char!(','), expr))
        ),
        |(o, elems)| Tuple::expr(elems, o)
    )
);

named!(primary_expr<Expr>, alt!(
    array |
    if_ |
    let_tuple |
    let_rec |
    let_ |
    tuple |
    put |
    apply |
    logical_or
));

named!(expr<Expr>, do_parse!(
    init: primary_expr >>
    folded:fold_many0!(
        do_parse!(
            o: offset >>
            ws!(char!(';')) >>
            rhs: primary_expr >>
            (o, rhs)
        ),
        init,
        |acc, (o, rhs)| Let::expr("_".to_string(), Box::new(acc), Box::new(rhs), o)
    ) >>
    (folded)
));

pub fn parse<S: AsRef<str>>(code: S) -> Result<AST, Error> {
    match expr(code.as_ref().as_bytes()) {
        IResult::Done(_, p) => Ok(AST {root: p}),
        IResult::Error(err) => Err(Error::OnParse { msg: format!("Parse error: {}", err) }),
        IResult::Incomplete(Needed::Size(n)) => Err(Error::OnParse { msg: format!("Parsed incomplete input: More {} bytes are needed", n) }),
        IResult::Incomplete(Needed::Unknown) => Err(Error::OnParse { msg: "Parsed incomplete input".to_string() }),
    }
}
