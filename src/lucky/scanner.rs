// use std::iter::IntoIterator;
use std::iter::Iterator;
use std::cmp::PartialEq;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub line: u32,
    pub column: u32,
    pub span: u32
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
                                TokenKind::Operator(Op::Equal),
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
                                TokenKind::Operator(Op::Equal),
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
                    '/' => {
                        self.character_next();
                        if self.it.clone().next() == Some('=') {
                            self.character_next();
                            Some(Token::new(TokenKind::Error(String::from("inplace modifying operators are not valid")), line, column, 0))
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
            NewLine => "newline".to_string(),
            OpenParen => "(".to_string(),
            CloseParen => ")".to_string(),
            OpenBrace => "[".to_string(),
            CloseBrace => "]".to_string(),
            OpenBracket => "{".to_string(),
            CloseBracket => "}".to_string(),
            BackSlash => "-".to_string(),
            Period => ".".to_string(),
            PeriodPeriod => "..".to_string(),
            PeriodPeriodPeriod => "...".to_string(),
            Comma => ",".to_string(),
            Colon => ":".to_string(),
            Semicolon => ";".to_string(),
            ColonColon => "::".to_string(),
            MinusGreater => "->".to_string(),
            // Hash => "#".to_string(),
            Dollar => "$".to_string(),
            At => "@".to_string(),
            Arrow => "=>".to_string(),
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Slash => "/".to_string(),
            Percent => "%".to_string(),
            Astrick => "*".to_string(),
            AstrickAstrick => "**".to_string(),
            LessLess => "<<".to_string(),
            GreaterGreater => ">>".to_string(),
            Ampersand => "&".to_string(),
            Pipe => "|".to_string(),
            Carrot => "^".to_string(),
            Tilde => "~".to_string(),
            Bang => "!".to_string(),
            Less => "<".to_string(),
            Greater => ">".to_string(),
            LessEqual => "<=".to_string(),
            GreaterEqual => ">=".to_string(),
            EqualEqual => "==".to_string(),
            BangEqual => "!=".to_string(),
            Equal => "=".to_string(),
            Underscore => "_".to_string(),
        }
    }
}

impl Kw {
    pub fn get_string(&self) -> String {
        match self {
            If => "if".to_string(),
            Else => "else".to_string(),
            Elif => "elif".to_string(),
            Let => "let".to_string(),
            Val => "val".to_string(),
            Type => "type".to_string(),
            Data => "data".to_string(),
            Struct => "struct".to_string(),
            Where => "where".to_string(),
            Foriegn => "foriegn".to_string(),
            Match => "match".to_string(),
            Use => "use".to_string(),
            And => "and".to_string(),
            Or => "or".to_string(),
            True => "true".to_string(),
            False => "false".to_string(),
            Ref => "ref".to_string()
        }
    }
}

#[derive(Clone, Debug)]
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
