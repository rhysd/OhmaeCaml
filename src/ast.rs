use std::char;

#[derive(Debug, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

pub trait GetOffset {
    fn get_offset(&self) -> usize;
}

pub fn get_pos<S: AsRef<str>, G: GetOffset>(s: S, g: G) -> Position {
    let bytes = s.as_ref().as_bytes();
    let offset = bytes.len() - g.get_offset();

    let mut l = 1usize;
    let mut c = 1usize;
    for b in &bytes[..offset] {
        match char::from_u32(*b as u32) {
            Some('\n') => {
                l += 1;
                c = 1;
            },
            Some(_) | None => {
                c += 1;
            }
        }
    }
    Position {line: l, column: c}
}

#[derive(Debug)]
pub struct Program {
    pub body: Vec<Expr>,
}

macro_rules! enum_nodes {
    ($($n:ident { $($m:ident,)+ })+) => {
        $(
            #[derive(Debug)]
            pub enum $n {
                $(
                    $m($m),
                )+
            }
            impl GetOffset for $n {
                fn get_offset(&self) -> usize {
                    match *self {
                        $(
                            $n::$m(ref m) => m.get_offset(),
                        )+
                    }
                }
            }
        )+
    }
}

enum_nodes! {
    Expr {
        Constant,
        BinOpExpr,
        UnaryOpExpr,
    }

    Constant {
        Num,
    }
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub struct Num {
    pub value: f64,
    pub offset: usize,
}

impl GetOffset for Num {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug)]
pub struct BinOpExpr {
    pub op: BinOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl GetOffset for BinOpExpr {
    fn get_offset(&self) -> usize {
        (*self.lhs).get_offset()
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Pos,
    Neg,
}

#[derive(Debug)]
pub struct UnaryOpExpr {
    pub op: UnaryOp,
    pub child: Box<Expr>,
}

impl GetOffset for UnaryOpExpr {
    fn get_offset(&self) -> usize {
        (*self.child).get_offset()
    }
}

