// mod self::ast;


use super::ast::{Expr, ExprKind, Decl, DeclKind, Type, TypeKind, Pos};
use super::scanner::{TokenStream, Token, TokenKind, Op, Kw};
use super::report::{Reporter, ReportPosition};


pub struct Parser<'a> {
    reporter: &'a mut Reporter,
    tokens: Vec<Token>,
    index: u32
}

type PResult<T> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn from_string(reporter: &'a mut Reporter, text: &str) -> Self {
        Self {
            reporter: reporter,
            tokens: TokenStream::from_string(text).collect::<Vec<_>>(),
            index: 0
        }
    }

    fn convert_position(pos: Pos) -> ReportPosition {
        // convert later to use the file if given.
        ReportPosition {
            line: pos.line,
            column: pos.column,
            span: pos.span,
            file: None
        }
    }

    fn convert_token(tok: &Token) -> ReportPosition {
        ReportPosition {
            line: tok.line,
            column: tok.column,
            span: tok.span,
            file: None
        }
    }

    fn current(&self) -> Option<Token> {
        if self.index < self.tokens.len() as u32 {
            Some(self.tokens[self.index as usize].clone())
        }
        else {
            None
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        match self.current() {
            Some(ref token) => token.kind == kind,
            _ => false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> bool {
        if self.check(kind.clone()) {
            self.next();
            true
        }
        else {
            match self.current() {
                Some(ref token) => {
                    self.reporter.report_error(Self::convert_token(token), format!("expected {} found {}", Token {kind: kind, line: 0, column: 0, span: 0 }.to_string(), token.to_string()));
                },
                _ => {
                    self.reporter.report_error_(ReportPosition {
                        line: 0,
                        column: 0,
                        span: 0,
                        file: None
                    }, "unexpected end of file");
                },
            }
            false
        }
    }

    fn allow(&mut self, kind: TokenKind) -> bool {
        if self.check(kind.clone()) {
            self.next();
            true
        }
        else {
            false
        }
    }

    fn next(&mut self) {
        self.index += 1
    }

    fn parse_unary(&mut self) -> PResult<Expr> {
        // println!("Unary {:?}", self.current());
        if let Some(token) = self.current() {
            match token.kind {
                TokenKind::Operator(ref op) => {
                    match *op {
                        Op::Bang | Op::Tilde | Op::Minus => {
                            self.next();
                            
                            let expr = self.parse_unary();
                            match expr {
                                Ok(e) =>  Ok(Expr::new(ExprKind::Unary(op.clone(), Box::new(e)), Pos { line: token.line, column: token.column, span: token.span + 1})),
                                _ => Err(()),
                            }
                        },
                        _ => self.parse_primary(),
                        
                    }
                },
                _ => self.parse_primary(),
            } 
        }
        else {
            Err(())
        }
    }

    fn parse_bottom(&mut self) -> PResult<Expr> {

        // println!("Parse bottom {:?}", self.current());
        let tok = self.current();
        if tok.is_none() {
            return Err(())
        }

        let token = tok.unwrap();

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
                // println!("Integer Literal");
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
            TokenKind::Operator(ref op) => {
                match *op {
                    Op::OpenParen => {
                        self.next();
                        let expr = self.parse_expr(None);

                        if self.expect(TokenKind::Operator(Op::CloseParen)) {
                            expr
                        }
                        else {
                            Err(())
                        }
                    },
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

    pub fn parse_expr(&mut self, min_prec_in: Option<u32>) -> PResult<Expr> {
        if let Ok(mut expr) = self.parse_unary() {
            let min_prec = match min_prec_in {
                Some(val) => val,
                None => 0,
            };
            // println!("Current Expression {:?}", expr);
            // println!("Current Token {:?}", self.current());

            while self.current().is_some() && (self.current().unwrap().precedence() >= min_prec) {
                let operator_ = self.current();

                // println!("TOperator {:?}", operator_);                

                if operator_.is_none() {
                    return Err(())
                }

                let operator = operator_.unwrap();

                let prec = operator.precedence();

                if operator.is_operator() {
                    self.next();

                    if let Ok(rhs) = self.parse_expr(Some(prec + 1)) {
                        let mut pos = expr.pos.clone();

                        pos.span += operator.span + rhs.pos.span;

                        if let TokenKind::Operator(ref op) = operator.kind {
                            let kind = ExprKind::Binary(op.clone(),
                                Box::new(expr),
                                Box::new(rhs));
                            expr = Expr::new(kind, pos);
                        }
                        else {
                            return Err(());
                        }
                    }
                    else {
                        println!("{:?}", operator);
                        let pos = Self::convert_token(&operator);
                        self.reporter.report_error(pos, format!("failed to find primary expression following '{}'", operator.to_string()));
                        return Err(())
                    }
                }
            }

            Ok(expr)
        }
        else {
            // println!("Error parsing unary expr");
            Err(())
        }
    }

}