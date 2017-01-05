#[derive(Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Expr>,
}

#[derive(Debug)]
pub enum Expr {
    Constant(Constant),
    BinOpExpr(BinOpExpr),
}

#[derive(Debug)]
pub enum Constant {
    Num(Num),
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div
}

pub fn bin_op_from_char(ch: char) -> BinOp {
    match ch {
        '+' => BinOp::Add,
        '-' => BinOp::Sub,
        '*' => BinOp::Mul,
        '/' => BinOp::Div,
        _   => panic!("Invalid binary operator {}", ch),
    }
}

#[derive(Debug)]
pub struct Num {
    pub value: f64,
    pub rest: usize,
}

#[derive(Debug)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

