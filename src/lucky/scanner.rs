// use std::iter::IntoIterator;
use std::iter::Iterator;
use std::cmp::PartialEq;
use std::string::ToString;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub column: u32,
    pub span: u32
}

impl ToString for Token {
    fn to_string(&self) -> String {
        match self.kind {
            // change this to a trait implementation  to_string()
            TokenKind::Operator(ref op) => op.get_string(),
            TokenKind::Keyword(ref kw) => kw.get_string(),
            TokenKind::Ident(ref val) => val.clone(),
            TokenKind::IntLiteral(i) => i.to_string(),
            TokenKind::FloatLiteral(f) => f.to_string(),
            TokenKind::StringLiteral(ref val) => val.clone(),
            TokenKind::NewLine => "newline".to_string(),
            TokenKind::Eof => "eof".to_string(),
            _ => "Error".to_string(),
        }
    }
}

impl Token {
    fn new(kind: TokenKind, line: u32, column: u32, span: u32) -> Token {
        Token {
            kind: kind,
            line: line,
            column: column,
            span: span
        }
    }

    pub fn precedence(&self) -> u32 {
        match &self.kind {
            TokenKind::Operator(ref op) => {
                match *op {
                    Op::AstrickAstrick => 12,
                    Op::Astrick | Op::Slash | Op::Percent => 11,
                    Op::Plus | Op::Minus => 10,
                    Op::LessLess | Op::GreaterGreater => 9,
                    Op::EqualEqual | Op::BangEqual => 8,
                    Op::LessEqual | Op::GreaterEqual |
                    Op::Less | Op::Greater => 7,
                    Op::Ampersand => 6,
                    Op::Carrot => 5,
                    Op::Pipe => 4,
                    Op::Equal => 1,
                    _ => 0,
                }
            },
            TokenKind::Keyword(ref kw) => {
                match *kw {
                    Kw::And => 3,
                    Kw::Or => 2,
                    _ => 0,
                }
            }
            _ => 0
        }
    }

    pub fn is_operator(&self) -> bool {
        match self.kind {
            TokenKind::Operator(ref op) => {
                match *op {
                    Op::Plus |
                    Op::Minus |
                    Op::Slash |
                    Op::Percent |
                    Op::Astrick |
                    Op::AstrickAstrick |
                    Op::LessLess |
                    Op::GreaterGreater |
                    Op::Ampersand |
                    Op::Pipe |
                    Op::Carrot |
                    Op::Less |
                    Op::Greater |
                    Op::LessEqual |
                    Op::GreaterEqual |
                    Op::EqualEqual |
                    Op::BangEqual |
                    Op::Equal => true,
                    _ => false,
                }
            },
            _ => false
        }
    }
}

// #[derive(Iterator)]
pub struct TokenStream<'a> {
    it: std::str::Chars<'a>,
    index: u32,
    line: u32,
    column: u32,
}

impl<'a> TokenStream<'a> {
    pub fn from_string(text: &'a str) -> TokenStream<'a>{
        TokenStream::<'a> {
            it: text.chars(),
            index: 0,
            line: 1,
            column: 1
        }
    }
    fn character_next(&mut self) {
        self.index += 1;
        self.column += 1;
        self.it.next();
    }

    fn get_token_kind(ident: &'a str) -> TokenKind {
        match ident {
            "if" => TokenKind::Keyword(Kw::If),
            "else" => TokenKind::Keyword(Kw::Else),
            "elif" => TokenKind::Keyword(Kw::Elif),
            "let" => TokenKind::Keyword(Kw::Let),
            "val" => TokenKind::Keyword(Kw::Val),
            "type" => TokenKind::Keyword(Kw::Type),
            "data" => TokenKind::Keyword(Kw::Data),
            "where" => TokenKind::Keyword(Kw::Where),
            "foriegn" => TokenKind::Keyword(Kw::Foriegn),
            "match" => TokenKind::Keyword(Kw::Match),
            "use" => TokenKind::Keyword(Kw::Use),
            "and" => TokenKind::Keyword(Kw::And),
            "or" => TokenKind::Keyword(Kw::Or),
            "true" => TokenKind::Keyword(Kw::True),
            "false" => TokenKind::Keyword(Kw::False),
            "ref" => TokenKind::Keyword(Kw::Ref),
            _ => TokenKind::Ident(String::from(ident)),
        }
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.it.clone().next() {
            Some(ch) => {
                let str = self.it.as_str();
                let line = self.line;
                let column = self.column;
                if ch.is_alphabetic() {
                    while self.it.clone().next().map_or(false, |x| x.is_alphanumeric()) {
                        self.character_next();
                    }

                    let kind = Self::get_token_kind(&str[..str.len() - self.it.as_str().len()]);

                    return Some(Token::new(
                        kind,
                        line,
                        column,
                        self.column - column)
                    );
                }

                match ch {
                    ' ' | '\t' => {
                        self.character_next();
                        self.next()
                    },
                    '"' => {
                        self.character_next();
                        let str2 = self.it.as_str();
                        while self.it.clone().next().map_or(false, |x| x != '"') {
                            self.character_next();
                        }

                        let kind = TokenKind::StringLiteral(String::from(&str2[..str2.len() - self.it.as_str().len()]));
                        self.character_next();

                        Some(Token::new(kind,
                            self.index, column,
                            self.column - column)
                        )
                    },
                    '\n' => {
                        self.line += 1;
                        self.column = 0;
                        self.character_next();
                        Some(Token::new(
                            TokenKind::NewLine,
                            line,
                            column,
                            1
                        ))
                    }
                    '0'...'9' => {
                        let mut float = false;
                        while self.it.clone().next().map_or(false, |x| x.is_numeric()) {
                            self.character_next();
                        }

                        if self.it.clone().next() == Some('.') {
                            float = true;
                            self.character_next();
                        }

                        while self.it.clone().next().map_or(false, |x| x.is_numeric()) {
                            self.character_next();
                        }

                        let num = String::from(&str[..str.len() - self.it.as_str().len()]);

                        let mut kind: TokenKind;

                        if float {
                            let n: f64 = num.parse::<f64>().unwrap();
                            kind = TokenKind::FloatLiteral(n);
                        }
                        else {
                            let n: i64 = num.parse::<i64>().unwrap();
                            kind = TokenKind::IntLiteral(n);                            
                        }

                        Some(Token::new(
                            kind,
                            line,
                            column,
                            self.column - column))

                    },
                    '+' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Plus),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '-' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Minus),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '*' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
                        }
                        else if self.it.clone().next() == Some('*') {
                            self.character_next();
                            Some(Token::new(
                                TokenKind::Operator(Op::AstrickAstrick),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Astrick),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '/' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Slash),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '%' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Percent),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '~' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::Tilde),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '{' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::OpenBracket),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },

                    '}' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::CloseBracket),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '(' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::OpenParen),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    ')' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::CloseParen),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '[' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::OpenBrace),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    ']' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::CloseBrace),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '?' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::Question),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '.' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::Period),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    ':' => {
                        self.character_next();
                        Some(Token::new(
                            TokenKind::Operator(Op::Colon),
                            line,
                            column,
                            self.column - column
                            )
                        )
                    },
                    '=' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::EqualEqual),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Equal),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '<' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::LessEqual),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else if self.it.clone().next() == Some('>') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::LessLess),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Less),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '>' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::GreaterEqual),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else if self.it.clone().next() == Some('>') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::GreaterGreater),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Greater),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '!' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();

                            Some(Token::new(
                                TokenKind::Operator(Op::BangEqual),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                        else {
                            Some(Token::new(
                                TokenKind::Operator(Op::Bang),
                                line,
                                column,
                                self.column - column
                                )
                            )
                        }
                    },
                    '#' => {
                        self.character_next();
                        while self.it.clone().next().map_or(false, |x| x != '\n') {
                            self.character_next();
                        }
                        // this can ge modified to return the content of the comment
                        self.next()
                    },
                    _ => None
                }
            },
            None => None,
        }
    }
}

// pub struct Scanner<'a> {
//     text: &'a str,
//     ch: Option<char>,
//     token: Option<Token>
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    BackSlash,
    Period,
    PeriodPeriod,
    PeriodPeriodPeriod,
    Comma,
    Colon,
    Semicolon,
    ColonColon,
    MinusGreater,
    Hash,
    Dollar,
    At,
    Arrow,
    Plus,
    Minus,
    Slash,
    Percent,
    Astrick,
    AstrickAstrick,
    LessLess,
    GreaterGreater,
    Ampersand,
    Pipe,
    Carrot,
    Tilde,
    Bang,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    EqualEqual,
    BangEqual,
    Equal,
    Question,
    Underscore,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Kw {
    If,
    Else,
    Elif,
    Let,
    Val,
    Type,
    Data,
    Struct,
    Where,
    Foriegn,
    Match,
    Use,
    And,
    Or,
    True,
    False,
    Ref,
}

impl Op {
    pub fn get_string(&self) -> String {
        match self {
            Op::OpenParen => "(".to_string(),
            Op::CloseParen => ")".to_string(),
            Op::OpenBrace => "[".to_string(),
            Op::CloseBrace => "]".to_string(),
            Op::OpenBracket => "{".to_string(),
            Op::CloseBracket => "}".to_string(),
            Op::BackSlash => "-".to_string(),
            Op::Period => ".".to_string(),
            Op::PeriodPeriod => "..".to_string(),
            Op::PeriodPeriodPeriod => "...".to_string(),
            Op::Comma => ",".to_string(),
            Op::Colon => ":".to_string(),
            Op::Semicolon => ";".to_string(),
            Op::ColonColon => "::".to_string(),
            Op::MinusGreater => "->".to_string(),
            // &Hash => "#".to_string(),
            Op::Dollar => "$".to_string(),
            Op::At => "@".to_string(),
            Op::Arrow => "=>".to_string(),
            Op::Plus => "+".to_string(),
            Op::Minus => "-".to_string(),
            Op::Slash => "/".to_string(),
            Op::Percent => "%".to_string(),
            Op::Astrick => "*".to_string(),
            Op::AstrickAstrick => "**".to_string(),
            Op::LessLess => "<<".to_string(),
            Op::GreaterGreater => ">>".to_string(),
            Op::Ampersand => "&".to_string(),
            Op::Pipe => "|".to_string(),
            Op::Carrot => "^".to_string(),
            Op::Tilde => "~".to_string(),
            Op::Bang => "!".to_string(),
            Op::Less => "<".to_string(),
            Op::Greater => ">".to_string(),
            Op::LessEqual => "<=".to_string(),
            Op::GreaterEqual => ">=".to_string(),
            Op::EqualEqual => "==".to_string(),
            Op::BangEqual => "!=".to_string(),
            Op::Equal => "=".to_string(),
            Op::Underscore => "_".to_string(),
            Op::Question => "?".to_string(),
            _ => "None".to_string(),
        }
    }
}

impl Kw {
    pub fn get_string(&self) -> String {
        match self {
            Kw::If => "if".to_string(),
            Kw::Else => "else".to_string(),
            Kw::Elif => "elif".to_string(),
            Kw::Let => "let".to_string(),
            Kw::Val => "val".to_string(),
            Kw::Type => "type".to_string(),
            Kw::Data => "data".to_string(),
            Kw::Struct => "struct".to_string(),
            Kw::Where => "where".to_string(),
            Kw::Foriegn => "foriegn".to_string(),
            Kw::Match => "match".to_string(),
            Kw::Use => "use".to_string(),
            Kw::And => "and".to_string(),
            Kw::Or => "or".to_string(),
            Kw::True => "true".to_string(),
            Kw::False => "false".to_string(),
            Kw::Ref => "ref".to_string()
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    Operator(Op),
    Keyword(Kw),
    NewLine,
    Eof,
    Error(String)
}
