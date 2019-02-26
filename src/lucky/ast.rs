
use super::scanner::Op;

#[derive(Clone, Debug)]
pub struct Pos {
    pub line: u32,
    pub column: u32,
    pub span: u32
}

#[derive(Clone, Debug)]
pub struct Identifier(String, Pos);

#[derive(Clone, Debug)]
pub enum ExprKind {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Name(String),
    Binary(Op, Box<Expr>, Box<Expr>),
    Unary(Op, Box<Expr>),
    App(Identifier, Vec<Expr>),
    Access(Box<Expr>, Identifier),
    Lambda(Vec<Identifier>, Box<Expr>)
}

#[derive(Clone, Debug)]
pub struct Field(Identifier, Type);

#[derive(Clone, Debug)]
pub struct Variant(Identifier, Vec<Type>);

#[derive(Clone, Debug)]
pub enum DeclKind {
    Function(Vec<Field>, Box<Expr>),
    Local(Box<Expr>),
    Sum(Vec<Variant>),
    Product(Variant),
    Record(Vec<Field>)
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Name(Identifier),
    Tuple(Vec<Type>),
    Funct(Vec<Type>, Box<Type>),
}

#[derive(Clone, Debug)]
pub struct Expr {
    kind: ExprKind,
    pub pos: Pos
}

#[derive(Clone, Debug)]
pub struct Decl {
    kind: DeclKind,
    name: Identifier,
    pub pos: Pos
}

#[derive(Clone, Debug)]
pub struct Type {
    kind: TypeKind,
    pub pos: Pos
}

impl Expr {
    pub fn new(kind: ExprKind, pos: Pos) -> Expr {
        Expr {
            kind, pos
        }
    }
}

impl Decl {
    pub fn new(kind: DeclKind, name: Identifier, pos: Pos) -> Decl {
        Decl {
            kind, name, pos
        }
    }
}

impl Type {
    pub fn new(kind: TypeKind, pos: Pos) -> Type {
        Type {
            kind, pos
        }
    }
}