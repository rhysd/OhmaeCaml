use std::str;
use std::str::FromStr;

use nom::{
    IResult,
    digit,
    alpha,
    alphanumeric,
    Needed,
};

use ast::*;
use error::Error;

fn offset(input: &[u8]) -> IResult<&[u8], usize> {
    IResult::Done(input, input.len())
}

#[allow(dead_code)]
macro_rules! named_dbg {
    ($name:ident<$o:ty>, $submac:ident!( $($args:tt)* )) => (
        named!($name<$o>, dbg!(
            $submac!( $($args)* )
        ));
    );
}

named!(comment<()>, do_parse!(
    tag!("(*") >>
    take_until!("*)") >>
    ()
));

named!(whitespace<()>, do_parse!(
    one_of_bytes!(b" \t\n\r") >>
    ()
));

named!(spaces<()>, do_parse!(
    many1!(alt!(whitespace | comment)) >> ()
));

named!(opt_spaces<()>, do_parse!(
    many0!(alt!(whitespace | comment)) >> ()
));

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
                        complete!(consume_frac),
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

named!(keyword<&[u8]>, do_parse!(
    k: alt!(
        tag!("true") |
        tag!("false") |
        tag!("if") |
        tag!("then") |
        tag!("else") |
        tag!("let") |
        tag!("rec") |
        tag!("in")
    ) >>
    not!(peek!(alphanumeric)) >>
    (k)
));

named!(identifier<String>,
    map!(
        map_res!(
            recognize!(
                do_parse!(
                    not!(keyword) >>
                    alpha >>
                    many0!(alphanumeric) >>
                    ()
                )
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
            opt_spaces >>
            o: offset >>
            tag!(b"||") >>
            opt_spaces >>
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
            opt_spaces >> 
            o: offset >>
            tag!(b"&&") >>
            opt_spaces >>
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
            opt_spaces >>
            o: offset >>
            op: equal_op >>
            opt_spaces >>
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
            opt_spaces >>
            o: offset >>
            op: relational_op >>
            opt_spaces >>
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
            opt_spaces >>
            o: offset >>
            op: additive_op >>
            opt_spaces >>
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
            opt_spaces >>
            o: offset >>
            op: mult_op >>
            opt_spaces >>
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
        opt_spaces >>
        op: unary_op >>
        opt_spaces >>
        child: unary >>
        (UnaryOpExpr::expr(op, Box::new(child), o))
    ) |
    postfix
));

enum PostFix {
    Put(Expr, Expr),
    Apply(Vec<Expr>),
}

named!(put_postfix<PostFix>, do_parse!(
    ws!(char!('.')) >>
    ws!(char!('(')) >>
    index: expr >>
    ws!(char!(')')) >>
    ws!(tag!("<-")) >>
    rhs: expr >>
    (PostFix::Put(index, rhs))
));

named!(apply_postfix<PostFix>, do_parse!(
    spaces >>
    args: separated_nonempty_list!(spaces, paren_less_expr) >>
    (PostFix::Apply(args))
));

named!(postfix<Expr>, do_parse!(
    init: paren_less_expr >>
    folded: fold_many0!(
        alt!(put_postfix | apply_postfix),
        init,
        |lhs, pf| match pf {
            PostFix::Put(index, rhs) => Put::expr(Box::new(lhs), Box::new(index), Box::new(rhs), 0),
            PostFix::Apply(args) => Apply::expr(Box::new(lhs), args, 0),
        }
    ) >>
    (folded)
));

named!(array<Expr>, do_parse!(
    o: offset >>
    tag!("Array.create") >>
    spaces >>
    size: paren_less_expr >>
    spaces >>
    elem: paren_less_expr >>
    (Array::expr(Box::new(size), Box::new(elem), o))
));

named!(if_<Expr>, do_parse!(
    o: offset >>
    tag!("if") >>
    spaces >>
    cond: expr >>
    spaces >>
    tag!("then") >>
    spaces >>
    then: expr >>
    spaces >>
    tag!("else") >>
    expr >>
    otherwise: expr >>
    expr >>
    (If::expr(Box::new(cond), Box::new(then), Box::new(otherwise), o))
));

named!(let_tuple<Expr>, do_parse!(
    o: offset >>
    names: ws!(delimited!(char!('('), separated_nonempty_list!(char!(','), identifier), char!(')'))) >>
    opt_spaces >>
    char!('=') >>
    opt_spaces >>
    bound: expr >>
    spaces >>
    tag!("in") >>
    spaces >>
    body: expr >>
    (LetTuple::expr(names, Box::new(bound), Box::new(body), o))
));

named!(fundef<Fundef>, do_parse!(
    n: identifier >>
    p: many0!(do_parse!(spaces >> arg: identifier >> (arg))) >>
    spaces >>
    b: expr >>
    (Fundef { name: n, params: p, body: Box::new(b) })
));

named!(let_rec<Expr>, do_parse!(
    o: offset >>
    tag!("let") >>
    spaces >>
    tag!("rec") >>
    spaces >>
    fun: fundef >>
    spaces >>
    tag!("in") >>
    spaces >>
    body: expr >>
    (LetRec::expr(fun, Box::new(body), o))
));

named!(let_<Expr>, do_parse!(
    o: offset >>
    tag!("let") >>
    spaces >>
    name: identifier >>
    ws!(char!('=')) >>
    bound: expr >>
    spaces >>
    tag!("in") >>
    spaces >>
    body: expr >>
    (Let::expr(name, Box::new(bound), Box::new(body), o))
));

named!(primary_expr<Expr>, alt!(
    array |
    if_ |
    let_tuple |
    let_rec |
    let_ |
    logical_or
));

fn tuple_or_primary(head: Expr, mut tail: Vec<Expr>, offset: usize) -> Expr {
    if tail.len() == 0 {
        head
    } else {
        tail.insert(0, head);
        Tuple::expr(tail, offset)
    }
}

named!(tuple<Expr>, do_parse!(
    o: offset >>
    head: primary_expr >>
    tail: many0!(ws!(preceded!(char!(','), primary_expr))) >>
    (tuple_or_primary(head, tail, o))
));

named!(expr<Expr>, do_parse!(
    init: tuple >>
    folded:fold_many0!(
        do_parse!(
            o: offset >>
            ws!(char!(';')) >>
            rhs: tuple >>
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
