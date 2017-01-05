// Type t =
//   | Unit
//   | Bool of bool
//   | Int of int
//   | Float of float
//   | Not of t
//   | Neg of t
//   | Add of t * t
//   | Sub of t * t
//   | FNeg of t
//   | FAdd of t * t
//   | FSub of t * t
//   | FMul of t * t
//   | FDiv of t * t
//   | Eq of t * t
//   | LE of t * t
//   | If of t * t * t
//   | Let of (Id.t * Type.t) * t * t
//   | Var of Id.t
//   | LetRec of fundef * t
//   | App of t * t list
//   | Tuple of t list
//   | LetTuple of (Id.t * Type.t) list * t * t
//   | Array of t * t
//   | Get of t * t
//   | Put of t * t * t
// and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }

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
        If,
        Let,
        Var,
        Apply,
        Tuple,
        LetTuple,
        Array,
        Get,
        Put,
    }
}

#[derive(Debug, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    Int(i32),
    Float(f64),
    Unit,
}

#[derive(Debug)]
pub struct Constant {
    pub value: ConstantValue,
    pub offset: usize,
}

impl GetOffset for Constant {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
    FNeg,
}

#[derive(Debug)]
pub strut UnaryOpExpr {
    pub op: UnaryOp,
    pub child: Box<Expr>,
}

impl GetOffset for UnaryOpExpr {
    fn get_offset(&self) -> usize {
        (*self.child).get_offset()
    }
}

#[derive(Debug, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    FAdd,
    FSub,
    FMul,
    FDiv,
    Eq,
    LE,
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
pub struct If {
    pub cond: Box<Expr>,
    pub then_clause: Box<Expr>,
    pub else_clause: Box<Expr>,
}

impl GetOffset for If {
    fn get_offset(&self) -> usize {
        (*self.cond).get_offset()
    }
}

#[derive(Debug)]
pub struct Let<'a> {
    pub name: &'a str,
    // pub ty: Type,
    pub bound: Box<Expr>,
    pub body: Box<Expr>,
    pub offset: u32,
}

impl<'a> GetOffset for Let<'a> {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug)]
pub struct Var<'a> {
    pub name: &'a str,
    pub offset: u32,
}

impl<'a> GetOffset for Var<'a> {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

// TODO
// pub struct LetRec

#[derive(Debug)]
pub struct Apply {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

impl GetOffset for Apply {
    fn get_offset(&self) -> usize {
        (*self.callee).get_offset()
    }
}

#[derive(Debug)]
pub struct Tuple {
    pub elems: Vec<Expr>,
    pub offset: u32,
}

impl GetOffset for Tuple {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug)]
pub struct LetTuple<'a> {
    pub names: Vec<&'a str>,
    pub bound: Box<Expr>,
    pub body: Box<Expr>,
    pub offset: u32,
}

impl<'a> GetOffset for LetTuple<'a> {
    fn get_offset(&self) -> usize {
        self.offset
    }
}

#[derive(Debug)]
pub struct Array {
    size: Box<Expr>,
    elem: Box<Expr>,
}

impl GetOffset for Array {
    fn get_offset(&self) -> usize {
        (*self.size).get_offset()
    }
}

#[derive(Debug)]
pub struct Get {
    array: Box<Expr>,
    index: Box<Expr>,
}

impl GetOffset for Get {
    fn get_offset(&self) -> usize {
        (*self.array).get_offset()
    }
}

#[derive(Debug)]
pub struct Put {
    array: Box<Expr>,
    index: Box<Expr>,
    lhs: Box<Expr>,
}

impl GetOffset for Put {
    fn get_offset(&self) -> usize {
        (*self.array).get_offset()
    }
}






