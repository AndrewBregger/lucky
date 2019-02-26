// mod self::ast;


use super::ast::{Expr, ExprKind, Decl, DeclKind, Type, TypeKind, Pos};
use super::scanner::{TokenStream, Token, TokenKind, Op, Kw};


pub struct Parser {
    tokens: Vec<Token>,
    index: u32
}

type PResult<T> = Result<T, ()>;

impl Parser {
    pub fn from_string(text: &str) -> Parser {
        Parser {
            tokens: TokenStream::from_string(text).collect::<Vec<_>>(),
            index: 0
        }
    }

    fn current(&self) -> &Token {
        &self.tokens[self.index as usize]
    }

    fn next(&mut self) {
        self.index += 1
    }

    fn parse_unary(&mut self) -> PResult<Expr> {
        let token = self.current().clone();

        match token.kind {
            TokenKind::Operator(ref op) => {
                match op {
                    Op::Bang | Op::Tilde => {
                        self.next();
                        let expr = self.parse_unary();
                        match expr {
                            Ok(e) =>  Ok(Expr::new(ExprKind::Unary(op.clone(), Box::new(e)), Pos { line: token.line, column: token.column, span: token.span})),
                            _ => Err(()),
                        }
                    },
                    _ => self.parse_primary(),
                    
                }
            },
            _ => self.parse_primary(),
        } 
    }

    fn parse_bottom(&mut self) -> PResult<Expr> {
        let token = self.current().clone();
        self.next();
        match &token.kind {
            TokenKind::Ident(ref s) => {
                let kind = ExprKind::Name(s.clone());
                let pos = Pos {
                    line: token.line,
                    column: token.column,
                    span: token.span
                };
                Ok(Expr::new(kind, pos))
            },
            TokenKind::IntLiteral(i) => {
                let kind = ExprKind::Integer(*i);
                let pos = Pos {
                    line: token.line,
                    column: token.column,
                    span: token.span
                };
                Ok(Expr::new(kind, pos))
            },
            TokenKind::FloatLiteral(f) => {
                let kind = ExprKind::Float(*f);
                let pos = Pos {
                    line: token.line,
                    column: token.column,
                    span: token.span
                };
                Ok(Expr::new(kind, pos))
            },
            TokenKind::StringLiteral(ref s) => {
                let kind = ExprKind::Str(s.clone());
                let pos = Pos {
                    line: token.line,
                    column: token.column,
                    span: token.span
                };
                Ok(Expr::new(kind, pos))
            },
            TokenKind::Keyword(kw) => {
                match kw {
                    Kw::If => self.parse_if(),
                    Kw::True | Kw::False => {
                        let val = *kw == Kw::True;
                        let kind = ExprKind::Bool(val);
                        let pos = Pos {
                            line: token.line,
                            column: token.column,
                            span: token.span
                        };
                        Ok(Expr::new(kind, pos))
                    },
                    // Kw::Let => 

                    _ => Err(())
                }
            },
            _ => Err(())
        }
    }

    fn parse_if(&mut self) -> PResult<Expr> {
        Err(())
    }

    fn parse_primary(&mut self) -> PResult<Expr> {
       let expr = self.parse_bottom();
       expr 
    }

    pub fn parse_expr(&mut self, min_prec: Option<u32>) -> PResult<Expr> {
        let mut expr = self.parse_unary();

        while 
    }

}