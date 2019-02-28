// mod self::ast;


use super::ast::{Expr, ExprKind, Decl, DeclKind, Type, TypeKind, Pos, Identifier, Field, Variant};
use super::scanner::{TokenStream, Token, TokenKind, Op, Kw};
use super::report::{Reporter, ReportPosition};


pub struct Parser<'a> {
    reporter: &'a mut Reporter,
    tokens: Vec<Token>,
    index: u32,
    ignore_newline: bool
}

#[derive(Debug)]
pub enum Optional<P, S, E> {
    Primary(P),
    Secondary(S),
    Err(E)
}

type PResult<T> = Result<T, ()>;

impl<'a> Parser<'a> {
    pub fn from_string(reporter: &'a mut Reporter, text: &str) -> Self {
        Self {
            reporter: reporter,
            tokens: TokenStream::from_string(text).collect::<Vec<_>>(),
            index: 0,
            ignore_newline: false
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

    fn to_position(tok: &Token) -> Pos {
        Pos {
            line: tok.line,
            column: tok.column,
            span: tok.span
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

    fn peek(&self, dist: u32) -> Option<Token> {
        if self.index + dist < self.tokens.len() as u32 {
            Some(self.tokens[(self.index + dist) as usize].clone())
        }
        else {
            None
        }
    }

    fn check(&self, kind: TokenKind) -> bool {
        println!("{:?}", kind);
        match self.current() {
            Some(ref token) =>{
                println!("{:?}", token.kind);
                token.kind == kind
            },
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
                    self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file");
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
        if self.ignore_newline {
            loop {
                match self.current() {
                    Some(ref token) => {
                        if token.is_newline() {
                            continue;
                        }
                        else {
                            break;
                        }
                    },
                    _ => break,
                }
            }
        }
        else {
            self.index += 1
        }
    }

    fn parse_variant(&mut self) -> PResult<Variant> {
        if let Some(id) = self.parse_ident() {
            let mut types : Vec<Type> = Vec::new();
            let mut pos = id.pos();

            while self.can_be_type() {
                let ty = self.parse_type();
                match self.parse_type() {
                    Ok(ty) => {
                        pos.span += ty.pos.span;
                        types.push(ty);
                    },
                    _ => return Err(())
                }
            }

            Ok(Variant::new(id, types))
        }
        else {
            println!("{:?}", self.current());
            self.reporter.report_error_(ReportPosition::zero(), "expecting identifier");
            Err(())
        }
    }

    fn parse_field(&mut self) -> PResult<Field> {
        println!("{:?}", self.current());
        if let Some(id) = self.parse_ident() {
            let mut pos = id.pos();
            if self.expect(TokenKind::Operator(Op::Colon)) {
                match self.parse_type() {
                    Ok(ty) => {
                        Ok(Field::new(id, ty))
                    },
                    _ => Err(())
                }     
            }
            else {
                Err(())
            }
        }
        else {
            Err(())
        }
    }


    fn parse_data_type(&mut self, ident: Identifier) -> PResult<Decl> {
        if self.allow(TokenKind::Keyword(Kw::Type)) {
            // let old = self.ignore_newline;
            // self.ignore_newline = true;
            let mut fields : Vec<Variant> = Vec::new();
            let mut pos = ident.pos();

            match self.parse_variant() {
                Ok(var) => {
                    fields.push(var);
                }
                _ => {
                    self.reporter.report_error_(Self::convert_position(pos), "expecting type constructor");
                    return Err(())
                }
            }
            
            while self.allow(TokenKind::Operator(Op::Pipe)) {
                self.allow(TokenKind::NewLine);
                match self.parse_variant() {
                    Ok(var) => {
                        fields.push(var);
                    },
                    _ => break,
                }
            }


            //self.ignore_newline = old;

            if fields.len() == 1 {
                Ok(Decl::new(DeclKind::Product(fields[0 as usize].clone()), ident, pos))
            }
            else {
                Ok(Decl::new(DeclKind::Sum(fields), ident, pos))
            }
        }
        else if self.allow(TokenKind::Keyword(Kw::Data)) {
            let mut fields : Vec<Field> = Vec::new();
            let mut pos = ident.pos();

            if self.expect(TokenKind::Operator(Op::OpenBracket)) {
                while !self.check(TokenKind::Operator(Op::CloseBracket)) {
                    self.allow(TokenKind::NewLine);
                    match self.parse_field() {
                        Ok(var) => {
                            fields.push(var);
                        },
                        _ => break,
                    }

                    if !self.allow(TokenKind::Operator(Op::Comma)) {
                        break;
                    }
                }

                if self.expect(TokenKind::Operator(Op::CloseBracket)) {
                    Ok(Decl::new(DeclKind::Record(fields), ident, pos))
                }
                else {
                    Err(())
                }
            }
            else {
                Err(())
            }
        }
        else {
            self.reporter.report_error_(Self::convert_position(ident.pos()), "expecting 'type' or 'data' following '='");
            Err(())
        }
    }

    fn parse_function(&mut self, ident: Identifier) -> PResult<Decl> {
        Err(())
    }

    pub fn parse_decl(&mut self) -> PResult<Decl> {
        match self.parse_ident() {
            Some(ident) => {
               if self.check(TokenKind::Operator(Op::Equal)) {
                   self.next();
                   self.parse_data_type(ident)
               } 
               else {
                   self.parse_function(ident)
               }
            },
            _ => {
                match self.current() {
                    Some(ref token) => {
                        self.reporter.report_error(Self::convert_token(token), format!("expecting an identifier, found: '{}'", token.to_string()));
                        Err(())
                    },
                    _ => {
                        Err(())
                    }
                }
            }
        }
    }

    fn parse_ident(&mut self) -> Option<Identifier> {
        match self.current() {
            Some(ref token) => {
                match token.kind {
                    TokenKind::Ident(ref value) => {
                        self.next();
                        Some(Identifier::new(value, Self::to_position(token)))
                    }
                    _ => None,
                }
            }
            _ => None
        }
    }

    pub fn parse_decl_or_expr(&mut self) -> Optional<Decl, Expr, ()> {
        // save the string index
        let start = self.index;

        let expr = match self.parse_expr(None) {
            Ok(e) => e,
            _ => return Optional::Err(())
        };

        match self.current() {
            Some(ref token) => {
                let is_decl = match token.kind.clone() {
                    TokenKind::Operator(op) => {
                        match op {
                            Op::MinusGreater |
                            Op::Equal => true,
                            _ => false,
                        }
                    },
                    TokenKind::Keyword(kw) => {
                        match kw {
                            Kw::Type |
                            Kw::Data => true,
                            _ => false,
                        }
                    },
                    _ => false,
                };


                // reset the token index
                self.index = start;
                match self.parse_decl() {
                    Ok(decl) => Optional::Primary(decl),
                    _ => Optional::Err(())
                }
            },
            _ => Optional::Secondary(expr),
        }
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
            self.reporter.report_error(ReportPosition {
                        line: 0,
                        column: 0,
                        span: 0,
                        file: None
                    }, "found eof instead of expression".to_string());
            return Err(())
        }

        let token = tok.unwrap();

        self.next();
        match token.kind {
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
                let kind = ExprKind::Integer(i);
                let pos = Pos {
                    line: token.line,
                    column: token.column,
                    span: token.span
                };
                Ok(Expr::new(kind, pos))
            },
            TokenKind::FloatLiteral(f) => {
                let kind = ExprKind::Float(f);
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
            TokenKind::Keyword(ref kw) => {
                match kw {
                    Kw::If => self.parse_if(),
                    Kw::True | Kw::False => {
                        let val = *kw == Kw::True;
                        let kind = ExprKind::Bool(val);
                        let pos = Self::to_position(&token);
                        Ok(Expr::new(kind, pos))
                    },
                    // Kw::Let => 

                    _ => {
                        self.reporter.report_error(ReportPosition::zero(), "invalid keyword in primary expression".to_string());
                        Err(())
                    }
                }
            },
            TokenKind::Operator(op) => {
                match op {
                    Op::OpenParen => {
                        let old = self.ignore_newline;
                        self.ignore_newline = true;


                        let expr = self.parse_expr(None);
                        self.ignore_newline = old;
                        // println!("{:?}", expr);
                        if self.expect(TokenKind::Operator(Op::CloseParen)) {
                            expr
                        }
                        else {
                            Err(())
                        }
                    },
                    _ => {
                        self.reporter.report_error( ReportPosition::zero(), "invalid operator in primary expression".to_string());
                        Err(())
                    }
                }
            },
            _ => {
                    self.reporter.report_error(ReportPosition::zero(), "invalid token in primary expression".to_string());
                    Err(())
            }
        }
    }

    fn parse_if(&mut self) -> PResult<Expr> {
        Err(())
    }


    fn parse_primary(&mut self) -> PResult<Expr> {
        if let Ok(mut expr) = self.parse_bottom() {
            loop {
                match self.current() {
                    Some(ref token) => {
                        match token.kind {
                            TokenKind::Operator(ref op) => {
                                match op {
                                    Op::Colon => {
                                        self.next();
                                        if let Some(ref token) = self.current() {
                                            match token.kind {
                                                TokenKind::Ident(ref name) => {
                                                    let ident = self.parse_ident();
                                                    let mut pos = expr.pos.clone();
                                                    let kind = ExprKind::Access(Box::new(expr), self.parse_ident().unwrap());
                                                    pos.span += token.span;
                                                    expr = Expr::new(kind, pos);
                                                    continue;
                                                },
                                                _ => {
                                                    self.reporter.report_error(Self::convert_token(token), "expected identifier following ':'".to_string());
                                                    return Err(());
                                                }
                                            }
                                        }
                                        else {
                                            self.reporter.report_error_(ReportPosition::zero(), "unexpected eof following ':'");
                                            return Err(());
                                        }
                                    },
                                    Op::Period => {
                                        self.next();
                                        if let Some(ref token) = self.current() {
                                            match token.kind {
                                                TokenKind::Ident(ref name) => {
                                                    self.next();
                                                    let value = Expr::new(ExprKind::Name(name.clone()), Self::to_position(token));
                                                    let mut pos = expr.pos.clone();
                                                    let mut actuals : Vec<Expr> = Vec::new();
                                                    actuals.push(expr);
                                                    loop {
                                                        // parse the current expression
                                                        match self.parse_expr(None) {
                                                            Ok(expr) => {
                                                                pos.span += expr.pos.span;
                                                                actuals.push(expr);
                                                            },
                                                            _ => {
                                                                break;
                                                            }
                                                        }
                                                        // check if there is another expression following
                                                        if self.can_be_application() {
                                                            continue;
                                                        }
                                                        else {
                                                            break;
                                                        }
                                                    }
                                                    let kind = ExprKind::App(Box::new(value), actuals);
                                                    pos.span += token.span;
                                                    expr = Expr::new(kind, pos);
                                                    continue;
                                                },
                                                _ => {
                                                    self.reporter.report_error(Self::convert_token(token), "expected identifier following '.'".to_string());
                                                    return Err(());
                                                }
                                            }
                                        }
                                        else {
                                            self.reporter.report_error_(ReportPosition::zero(), "unexpected eof following ':'");
                                            return Err(());
                                        } 
                                    },
                                    _ => break,
                                }
                            },
                            _ => break,
                        }
                    },
                    None => {
                        break;
                    },
                }
            }
            // now check if this is an application
            if (expr.is_name() || expr.is_accessor()) && self.can_be_application() {
                let mut actuals : Vec<Expr> = Vec::new();
                let mut pos = expr.pos.clone();
                loop {
                    // parse the current expression
                    match self.parse_expr(None) {
                        Ok(expr) => {
                            pos.span += expr.pos.span;
                            actuals.push(expr);
                        },
                        _ => {
                            break;
                        }
                    }
                    // check if there is another expression following
                    if self.can_be_application() {
                        continue;
                    }
                    else {
                        break;
                    }
                }

                if actuals.is_empty() {
                    // maybe this should be an error
                    Ok(expr)
                }
                else {
                    let kind = ExprKind::App(Box::new(expr), actuals);
                    expr = Expr::new(kind, pos);
                    Ok(expr)
                }
            }
            else {
                Ok(expr)
            }
        }
        else {
            Err(())
        }
    }

    pub fn parse_expr(&mut self, min_prec_in: Option<u32>) -> PResult<Expr> {
        if let Ok(mut expr) = self.parse_unary() {
            let min_prec = match min_prec_in {
                Some(val) => val,
                None => 2,
            };
            // println!("Current Expression {:?}", expr);
            // println!("Current Token {:?}", self.current());

            while self.current().is_some() && (self.current().unwrap().precedence() >= min_prec) {
                let operator_ = self.current();

                println!("TOperator {:?}", operator_);                

                if operator_.is_none() {
                    self.reporter.report_error(ReportPosition::zero(), "unexpected eof".to_string()); 
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
                        // println!("{:?}", operator);
                        let pos = Self::convert_token(&operator);
                        self.reporter.report_error(pos, format!("failed to find primary expression following '{}'", operator.to_string()));
                        return Err(())
                    }
                }
                else {
                    break;
                }
            }

            Ok(expr)
        }
        else {
            // println!("Error parsing unary expr");
            Err(())
        }
    }

    fn parse_type(&mut self) -> PResult<Type> {
        match self.current() {
            Some(ref token) => {
                match token.kind {
                    TokenKind::Ident(ref value) => {
                        match self.parse_ident() {
                            Some(ident) => {
                                let mut pos = Self::to_position(token);

                                if self.allow(TokenKind::Operator(Op::OpenBrace)) {
                                    let mut types : Vec<Type> = Vec::new();

                                    match self.parse_type() {
                                        Ok(ty) => {
                                            pos.span += ty.pos.span;
                                            types.push(ty);
                                        },
                                        _ => return Err(())
                                    }

                                    while self.allow(TokenKind::Operator(Op::Comma)) {
                                        match self.parse_type() {
                                            Ok(ty) => {
                                                pos.span += ty.pos.span;
                                                types.push(ty);
                                            },
                                            _ => return Err(())
                                        }
                                    }

                                    if self.expect(TokenKind::Operator(Op::CloseBrace)) {
                                        Ok(Type::new(TypeKind::Polymorphic(ident, types), pos))
                                    }
                                    else {
                                        Err(())
                                    }
                                }
                                else {
                                    Ok(Type::new(TypeKind::Name(ident), pos))
                                }
                            },
                            _ => Err(())
                        }
                    },
                    TokenKind::Operator(ref op) => {
                        match op {
                            Op::OpenParen => {
                                self.next();
                                let old = self.ignore_newline;
                                self.ignore_newline = true;
                                let mut fields = Vec::new();
                                let mut pos = Self::to_position(token);

                                match self.parse_type() {
                                    Ok(ty) => {
                                        pos.span += ty.pos.span;
                                        fields.push(ty)
                                    },
                                    _ => {
                                        match self.current() {
                                            Some(ref token) => {
                                                self.reporter.report_error(Self::convert_token(token), format!("failed to find type specification following '(', found: '{}'", token.to_string()));
                                            }
                                            _ => {
                                                self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file when parsing type specification");
                                            }
                                        }
                                        return Err(());
                                    }
                                }

                                while self.allow(TokenKind::Operator(Op::Comma)) {
                                    match self.parse_type() {
                                        Ok(ty) => {
                                            pos.span += ty.pos.span;
                                            fields.push(ty)
                                        },
                                        _ => {
                                            match self.current() {
                                                Some(ref token) => {
                                                    self.reporter.report_error(Self::convert_token(token), format!("failed to find type specification following ',', found: '{}'", token.to_string()));
                                                }
                                                _ => {
                                                    self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file when parsing type specification");
                                                }
                                            }
                                            return Err(());
                                        }
                                    }
                                }

                                self.ignore_newline = old;
                                if self.expect(TokenKind::Operator(Op::CloseParen)) {
                                    pos.span += 1;
                                    Ok(Type::new(TypeKind::Tuple(fields), pos))
                                }
                                else {
                                    Err(())
                                }
                            },
                            Op::OpenBrace => {
                                self.next();
                                match self.parse_type() {
                                    Ok(ty) => {
                                        if self.expect(TokenKind::Operator(Op::CloseBrace)) {
                                            let pos = ty.pos.clone();
                                            Ok(Type::new(TypeKind::List(Box::new(ty)), pos))
                                        }
                                        else {
                                            Err(())
                                        }
                                    },
                                    _ => {
                                        match self.current() {
                                            Some(ref token) => {
                                                self.reporter.report_error(Self::convert_token(token), format!("failed to find type specification following '['"));
                                                Err(())
                                            }
                                            _ => {
                                                self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file when parsing type specification");
                                                Err(())
                                            }
                                        }
                                    }
                                }

                            },
                            Op::Dollar => {
                                self.next();
                                match self.parse_ident() {
                                    Some(id) => {
                                        let pos = id.pos();
                                        Ok(Type::new(TypeKind::Generic(id), pos)
                                    }
                                    _ => {
                                        // this is a recurring pattern mabye make this a wrapping function
                                        match self.current() {
                                            Some(ref token) => {
                                                self.reporter.report_error(Self::convert_token(token), format!("expecting identifier, found: '{}'", token.to_string()));
                                                Err(())
                                            }
                                            _ => {
                                                self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file when parsing type specification");
                                                Err(())
                                            }
                                        }
                                    }
                                }
                            }
                            _ => {
                                match self.current() {
                                    Some(ref token) => {
                                        self.reporter.report_error(ReportPosition::zero(), format!("unexpected token when parsing a type: found '{}'", token.to_string()));
                                        Err(())
                                    },
                                    _ => {
                                        self.reporter.report_error_(ReportPosition::zero(), "unexpected end of file when parsing type specification");
                                        Err(())
                                    }
                                }
                            }
                        }
                    },
                    _ => {
                        self.reporter.report_error(Self::convert_token(token), format!("expecting '[', '$', '(', or identifier: found: '{}'", token.to_string()));
                        Err(())
                    }
                }
            },
            _ => {
                self.reporter.report_error_(ReportPosition::zero(), "unexpected eof when expecting type signigure");
                Err(())
            }
        }
    }

    fn can_be_type(&self) -> bool {
        match self.current() {
            Some(ref token) => {
                match token.kind {
                    TokenKind::Ident(_) => true,
                    TokenKind::Operator(ref op) => {
                        match op {
                            Op::OpenParen |
                            Op::OpenBrace |
                            Op::Dollar => true,
                            _ => false,
                        }
                    } ,
                    _ => false,
                }
            },
            _ => false
        }
    }

    fn can_be_application(&self) -> bool {
        match self.current() {
            Some(ref token) => {
                match token.kind {
                    TokenKind::IntLiteral(_) |
                    TokenKind::FloatLiteral(_) |
                    TokenKind::StringLiteral(_) |
                    TokenKind::Ident(_) => true,
                    TokenKind::Operator(ref op) => {
                        match *op {
                            Op::OpenParen => true,
                            _ => false,
                        }
                    },
                    TokenKind::Keyword(ref kw) => {
                        match *kw {
                            Kw::True |
                            Kw::False => true,
                            _ => false,
                        }
                    }
                    _ => false,
                } 
            },
            _ => false,
        }
    }    
}