#![allow(dead_code)]

use std::{error::Error, fs::read_to_string, process::exit};

struct Iter<T> {
    items: Vec<T>,
    idx: usize,
    stack: Vec<usize>,
}

impl<T: Clone> Iter<T> {
    fn new<I: IntoIterator<Item = T>>(items: I) -> Self {
        Self {
            items: items.into_iter().collect(),
            idx: 0,
            stack: vec![],
        }
    }

    fn peek(&self) -> Option<T> {
        self.peek_many(1)
    }

    fn peek_many(&self, amount: usize) -> Option<T> {
        self.items.get(self.idx + amount - 1).cloned()
    }

    fn next(&mut self) -> T {
        self.try_next().unwrap()
    }

    fn try_next(&mut self) -> Option<T> {
        let res = self.peek()?;
        self.idx += 1;
        Some(res)
    }

    fn back(&mut self) {
        self.back_many(1);
    }

    fn back_many(&mut self, amount: usize) {
        self.idx -= amount;
    }

    fn take_while<F: Fn(T) -> bool>(&mut self, f: F) -> Vec<T> {
        let mut list = vec![];
        while self.peek().map(&f).unwrap_or(false) {
            list.push(self.next());
        }
        list
    }

    fn empty(&self) -> bool {
        self.peek().is_none()
    }

    fn remaining(&self) -> usize {
        self.items.len() - self.idx
    }

    fn save(&mut self) {
        self.stack.push(self.idx);
    }

    fn restore(&mut self) {
        self.idx = self.stack.pop().expect("Iter stack underflow");
    }

    fn commit(&mut self) {
        self.stack.pop().expect("Iter stack underflow");
    }
}

impl Iter<char> {
    fn str_take_while<F: Fn(char) -> bool>(&mut self, f: F) -> String {
        self.take_while(f).into_iter().collect()
    }
}

#[derive(PartialEq, Clone, Debug, Copy)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub len: usize,
    pub kind: TokenKind,
}

impl Token {
    fn new(i: &Iter<char>, start: usize, line: usize, line_start: usize, kind: TokenKind) -> Token {
        Token {
            line,
            column: i.idx - line_start,
            len: i.idx - start,
            kind,
        }
    }

    fn new_(
        i: &Iter<char>,
        start: usize,
        line: usize,
        line_start: usize,
        kind: Option<TokenKind>,
    ) -> Token {
        Token {
            line,
            column: i.idx - line_start,
            len: i.idx - start,
            kind: kind.unwrap_or(TokenKind::Error),
        }
    }

    fn loc(&self, input_path: &str) -> String {
        format!("{}:{}:{}: ", input_path, self.line, self.column)
    }

    fn string(&self, input_path: &str) -> String {
        format!("{}{:#?}", self.loc(input_path), self.kind)
    }
}

#[derive(PartialEq, Clone, Debug, Copy)]
pub enum TokenKind {
    LPar,
    RPar,
    Semi,
    Eq,
    EqEq,
    Ident,
    Integer,
    Error,
    If,
    Else,
    While,
}

const SYMBOLS: &[(&str, TokenKind)] = &[
    ("(", TokenKind::LPar),
    (")", TokenKind::RPar),
    (";", TokenKind::Semi),
    ("=", TokenKind::Eq),
    ("==", TokenKind::EqEq),
];

const KEYWORDS: &[(&str, TokenKind)] = &[
    ("if", TokenKind::If),
    ("else", TokenKind::Else),
    ("while", TokenKind::While),
];

fn is_space(c: char) -> bool {
    c == '\r' || c == ' ' || c == '\t'
}

fn is_alphanum(c: char) -> bool {
    is_alpha(c) || is_num(c)
}

fn is_alpha(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_num(c: char) -> bool {
    c.is_ascii_digit()
}

macro_rules! diag {
    ($token:expr, $input_path:expr, $($fmt:tt)*) => {
        eprint!("{}", $token.loc($input_path));
        eprintln!($($fmt)*);
    };
}

fn imatch<T: Clone + PartialEq, I: IntoIterator<Item = T>>(i: &Iter<T>, l: I) -> bool {
    l.into_iter()
        .enumerate()
        .all(|(n, e)| i.peek_many(n) == Some(e))
}

fn tokenize(source: &str, input_path: &str) -> Vec<Token> {
    let mut i = Iter::new(source.chars());
    let mut tokens = vec![];
    let mut line = 1;
    let mut line_start = 0;

    let mut symbols = SYMBOLS.to_vec();
    symbols.sort_by_key(|(k, _)| k.len());
    symbols.reverse();

    loop {
        i.take_while(is_space);
        let start = i.idx;
        let Some(c) = i.try_next() else { break };

        if c == '\n' {
            line += 1;
            line_start = i.idx;
            continue;
        }

        if !is_alphanum(c) {
            let Some(&(str, kind)) = symbols.iter().find(|&(k, _)| imatch(&i, k.chars())) else {
                let token = Token::new_(&i, start, line, line_start, None);
                diag!(token, input_path, "Unknown symbol `{}`", c);
                exit(1);
            };
            let token = Token::new(&i, start, line, line_start, kind);
            i.back();
            i.idx += str.len();
            tokens.push(token);
            continue;
        }

        if is_alpha(c) {
            i.back();
            let str = i.str_take_while(is_alphanum);
            let kind = KEYWORDS
                .iter()
                .find_map(|&(k, v)| (k == str).then(|| v))
                .unwrap_or(TokenKind::Ident);
            let token = Token::new(&i, start, line, line_start, kind);
            tokens.push(token);
            continue;
        }

        if !is_num(c) {
            unreachable!("{c:#?}")
        }

        i.back();
        i.str_take_while(is_num);
        let token = Token::new(&i, start, line, line_start, TokenKind::Integer);
        tokens.push(token);

        if let Some(c) = i.peek()
            && is_alpha(c)
        {
            let token = Token::new(&i, start, line, line_start, TokenKind::Error);
            diag!(token, input_path, "Unexpected char after integer `{}`", c);
            exit(1);
        }
    }
    tokens
}

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = "main.bp";
    let source = read_to_string(input_path)?;
    let tokens = tokenize(&source, input_path);
    for token in &tokens {
        println!("{}", token.string(input_path));
    }

    Ok(())
}
