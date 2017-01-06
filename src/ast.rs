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

use std::char;

pub trait GetOffset {
    fn get_offset(&self) -> usize;
}

macro_rules! enum_ast_nodes {
    ($($n:ident { $($m:ident { $($f:ident: $t:ty,)* })+ })+) => {
        $(
            $(
                #[derive(Debug)]
                pub struct $m {
                    $($f: $t,)*
                    pub offset: usize,
                }
                impl GetOffset for $m {
                    #[inline]
                    fn get_offset(&self) -> usize {
                        self.offset
                    }
                }
            )+

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

// Note:
// MinCaml only has expression nodes

enum_ast_nodes! {
    Expr {
        Constant {
            value: ConstantValue,
        }

        BinOpExpr {
            op: BinOp,
            lhs: Box<Expr>,
            rhs: Box<Expr>,
        }

        UnaryOpExpr {
            op: UnaryOp,
            child: Box<Expr>,
        }

        If {
            cond: Box<Expr>,
            then_clause: Box<Expr>,
            else_clause: Box<Expr>,
        }

        Let {
            name: String,
            bound: Box<Expr>,
            body: Box<Expr>,
        }

        Var {
            name: String,
        }

        LetRec {
            funcdef: Box<Fundef>, // It should be Rc
            body: Box<Expr>,
        }

        Apply {
            callee: Box<Expr>,
            args: Vec<Expr>,
        }

        Tuple {
            elems: Vec<Expr>,
        }

        LetTuple {
            names: Vec<String>,
            bound: Box<Expr>,
            body: Box<Expr>,
        }

        Array {
            size: Box<Expr>,
            elem: Box<Expr>,
        }

        Get {
            array: Box<Expr>,
            index: Box<Expr>,
        }

        Put {
            array: Box<Expr>,
            index: Box<Expr>,
            rhs: Box<Expr>,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ConstantValue {
    Bool(bool),
    Int(i32),
    Float(f64),
    Unit,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
    FNeg,
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
pub struct Fundef {
    pub name: String,
    pub args: Vec<String>,
    pub body: Box<Expr>,
}
