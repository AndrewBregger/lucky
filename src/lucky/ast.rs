
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
    App(Box<Expr>, Vec<Expr>),
    // operand:name
    Access(Box<Expr>, Identifier),
    Lambda(Vec<Identifier>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Field(Identifier, Option<Type>);

#[derive(Clone, Debug)]
pub struct Variant(Identifier, Vec<Type>);

#[derive(Clone, Debug)]
pub struct FunctionSignature(Vec<Field>, Option<Box<Type>>);

#[derive(Clone, Debug)]
pub enum DeclKind {
    Function(FunctionSignature, Box<Expr>),
    Local(Box<Expr>),
    Sum(Vec<Variant>),
    Product(Variant),
    Record(Vec<Field>)
}

#[derive(Clone, Debug)]
pub enum TypeKind {
    Name(Identifier),
    Polymorphic(Identifier, Vec<Type>),
    Tuple(Vec<Type>),
    Funct(Vec<Type>, Box<Type>),
    Generic(Identifier),
    List(Box<Type>)
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

    pub fn is_name(&self) -> bool {
        match self.kind {
            ExprKind::Name(_) => true,
            _ => false,
        }
    }

    pub fn is_binary(&self) -> bool {
        match self.kind {
            ExprKind::Binary(..) => true,
            _ => false,
        }
    }

    pub fn is_unary(&self) -> bool {
        match self.kind {
            ExprKind::Unary(..) => true,
            _ => false,
        }
    }

    pub fn is_application(&self) -> bool {
        match self.kind {
            ExprKind::App(..) => true,
            _ => false,
        }
    }

    pub fn is_accessor(&self) -> bool {
        match self.kind {
            ExprKind::Access(..) => true,
            _ => false,
        }
    }

    pub fn is_literal(&self) -> bool {
        match self.kind {
            ExprKind::Integer(_) |
            ExprKind::Float(_) |
            ExprKind::Bool(_) |
            ExprKind::Str(_) => true,
            _ => false,
        }
    }

    pub fn is_lambda(&self) -> bool {
        match self.kind {
            ExprKind::Lambda(..) => true,
            _ => false,
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
            kind,
            pos
        }
    }
}

impl Identifier {
    pub fn new(val: &String, pos: Pos) -> Self {
        Identifier(val.clone(), pos)
    }

    pub fn pos(&self) -> Pos {
        self.1.clone()
    }
}

impl Field {
    pub fn new(id: Identifier, ty: Option<Type>) -> Self {
        Self(id, ty)
    }
}

impl Variant {
    pub fn new(id: Identifier, tys: Vec<Type>) -> Self {
        Self(id, tys)
    }
}

impl FunctionSignature {
    pub fn new(fields: Vec<Field>, ret: Option<Box<Type>>) -> Self {
        Self(fields, ret)
    }
}