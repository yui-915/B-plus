#![allow(dead_code)]

use std::{borrow::Borrow, error::Error, fs::read_to_string};

#[cfg(test)]
mod tests;

struct Iter<T> {
    items: Vec<T>,
    idx: usize,
    stack: Vec<usize>,
}

trait Boxed<T> {
    fn boxed(self) -> Box<T>;
}

impl<T> Boxed<T> for T {
    fn boxed(self) -> Box<T> {
        Box::new(self)
    }
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

#[derive(PartialEq, Clone, Debug)]
struct Token {
    input_path: String,
    line: usize,
    column: usize,
    start: usize,
    len: usize,
    kind: TokenKind,
}

impl Token {
    fn new(
        i: &Iter<char>,
        input_path: &str,
        start: usize,
        line: usize,
        line_start: usize,
        kind: TokenKind,
    ) -> Token {
        Token {
            input_path: input_path.to_owned(),
            line,
            column: i.idx - line_start,
            start,
            len: i.idx - start,
            kind,
        }
    }

    fn new_(
        i: &Iter<char>,
        input_path: &str,
        start: usize,
        line: usize,
        line_start: usize,
        kind: Option<TokenKind>,
    ) -> Token {
        Token {
            input_path: input_path.to_owned(),
            line,
            column: i.idx - line_start,
            start,
            len: i.idx - start,
            kind: kind.unwrap_or(TokenKind::Error),
        }
    }

    fn loc(&self) -> String {
        format!("{}:{}:{}: ", self.input_path, self.line, self.column)
    }

    fn loc_string(&self) -> String {
        format!("{}{:#?}", self.loc(), self.kind)
    }

    fn str(&self, c: &Compiler) -> String {
        c.source[self.start..self.start + self.len].to_owned()
    }
}

macro_rules! gen_token_kind {
    {
        symbols  {$( $symbol:ident  $symbol_str:literal  )*}
        keywords {$( $keyword:ident $keyword_str:literal )*}
        other    {$( $other:ident                        )*}
    } => {
        #[derive(PartialEq, Clone, Debug, Copy)]
        pub enum TokenKind {
            $($symbol,)*
            $($keyword,)*
            $($other,)*
        }
        const SYMBOLS: &[(&str, TokenKind)] = &[$(
            ($symbol_str, TokenKind::$symbol),
        )*];

        const KEYWORDS: &[(&str, TokenKind)] = &[$(
            ($keyword_str, TokenKind::$keyword),
        )*];
    };
}

gen_token_kind! {
    symbols {
        LPar   "(" RPar   ")"
        LBrak  "[" RBrak  "]"
        LBrace "{" RBrace "}"

        Plus  "+"
        Minus "-"
        Star  "*"
        Slash "\\"
        Mod   "%"

        PlusPlus "++" MinusMinus "--"

        And "&"  Or "|"   Xor "^"   Tilde "~"
        Shl "<<" Shr ">>" Sar ">>>"

        AndAnd "&&" OrOr "||" Not "!"

        EqEq "==" NotEq "!="
        Lt   "<"  Gt   ">"
        LtEq "<=" GtEq ">="

        Question "?" Colon ":"

        Comma ","
        Semi  ";"
        Eq    "="

        PlusEq "+=" MinusEq "-="  StarEq "*="   DivEq "/=" PrecentEq "%="
        ShlEq "<<=" ShrEq   ">>=" SarEq  ">>>="
        AndEq "&="  PipeEq  "|="  XorEq  "^="   TildeEq "~="
    }
    keywords {
        If    "if"
        Else  "else"
        While "while"
    }
    other {
        Ident
        Integer
        // Float
        // String
        // Char
        Error
        Eof
    }
}

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
    ($token:expr, $($fmt:tt)*) => {
        eprint!("{}", $token.loc());
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
                let token = Token::new_(&i, input_path, start, line, line_start, None);
                diag!(token, "Unknown symbol `{}`", c);
                panic!();
                // exit(1);
            };
            let token = Token::new(&i, input_path, start, line, line_start, kind);
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
            let token = Token::new(&i, input_path, start, line, line_start, kind);
            tokens.push(token);
            continue;
        }

        if !is_num(c) {
            unreachable!("{c:#?}")
        }

        i.back();
        i.str_take_while(is_num);
        let token = Token::new(&i, input_path, start, line, line_start, TokenKind::Integer);
        tokens.push(token);

        if let Some(c) = i.peek()
            && is_alpha(c)
        {
            let token = Token::new(&i, input_path, start, line, line_start, TokenKind::Error);
            diag!(token, "Unexpected char after integer `{}`", c);
            panic!();
            // exit(1);
        }
    }

    i.idx += 1;
    tokens.push(Token::new(
        &i,
        input_path,
        i.idx - 1,
        line,
        line_start,
        TokenKind::Eof,
    ));

    tokens
}

impl Value {
    fn string(&self) -> String {
        match self {
            Value::Integer(int) => format!("{int}"),
        }
    }
}

impl Infix {
    fn string(&self) -> String {
        use Infix::*;
        match self {
            Sub => "-",
            Add => "+",
            Mul => "*",
            Index => "[",
            FunCall => "f",
        }
        .to_owned()
    }
}

impl Prefix {
    fn string(&self) -> String {
        use Prefix::*;
        match self {
            Negate => "-",
            Increment => "++",
        }
        .to_owned()
    }
}

impl Postfix {
    fn string(&self) -> String {
        use Postfix::*;
        match self {
            Increment => "++",
        }
        .to_owned()
    }
}

impl Expr {
    fn string(&self) -> String {
        match self {
            Expr::Atom(value) => value.string(),
            Expr::Infix(infix, operands) => {
                format!(
                    "({} {} {})",
                    infix.string(),
                    operands.0.string(),
                    operands.1.string()
                )
            }
            Expr::Prefix(prefix, operand) => {
                format!("({} {})", prefix.string(), operand.string(),)
            }
            Expr::Postfix(postfix, operand) => {
                format!("({} {})", postfix.string(), operand.string(),)
            }
        }
    }
}

enum Expr {
    Atom(Value),
    Infix(Infix, Box<(Expr, Expr)>),
    Prefix(Prefix, Box<Expr>),
    Postfix(Postfix, Box<Expr>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Prefix {
    Negate,
    Increment,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Postfix {
    Increment,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Infix {
    Add,
    Sub,
    Mul,
    Index,
    FunCall,
}

enum Value {
    Integer(u64),
}

fn parse_value(t: Token, c: &mut Compiler) -> Expr {
    // TODO: ident
    // TODO: no unwrap
    // TODO: automate using macro
    let value = match t.kind {
        TokenKind::Integer => Value::Integer(t.str(c).parse().unwrap()),
        _ => {
            diag!(
                t,
                "Unexpected token `{:?}` while trying to parse value",
                t.kind
            );
            panic!();
            // exit(1);
        }
    };

    Expr::Atom(value)
}

fn parse_infix(t: Token, _c: &mut Compiler) -> Infix {
    // TODO: automate using macro
    match t.kind {
        TokenKind::Plus => Infix::Add,
        TokenKind::Star => Infix::Mul,
        TokenKind::Minus => Infix::Sub,
        _ => {
            diag!(
                t,
                "Unexpected token `{:?}` while trying to parse infix",
                t.kind
            );
            panic!();
            // exit(1);
        }
    }
}

fn try_parse_prefix<T: Borrow<Token>>(t: T, _c: &mut Compiler) -> Option<Prefix> {
    // TODO: automate using macro
    match t.borrow().kind {
        TokenKind::Minus => Some(Prefix::Negate),
        TokenKind::PlusPlus => Some(Prefix::Increment),
        _ => None,
    }
}

fn try_parse_postfix<T: Borrow<Token>>(t: T, _c: &mut Compiler) -> Option<Postfix> {
    // TODO: automate using macro
    match t.borrow().kind {
        TokenKind::PlusPlus => Some(Postfix::Increment),
        _ => None,
    }
}

fn infix_bp(i: Infix) -> (f32, f32) {
    use Infix::*;
    match i {
        Add | Sub => (1., 1.1),
        Mul => (2., 2.1),
        Index | FunCall => unreachable!(),
    }
}
const PREFIX_BP: f32 = 3.;

fn parse_expr(c: &mut Compiler, bp: f32, end: TokenKind) -> Expr {
    let t = c.tokens.next();
    let mut lhs = match t.kind {
        TokenKind::LPar => {
            let lhs = parse_expr(c, 0., TokenKind::RPar);
            c.tokens.next();
            lhs
        }
        _ if try_parse_prefix(&t, c).is_some() => {
            // TODO: no unwrap
            let prefix = try_parse_prefix(t, c).unwrap();
            let operand = parse_expr(c, PREFIX_BP, end);
            Expr::Prefix(prefix, operand.boxed())
        }
        _ => parse_value(t, c),
    };

    loop {
        // TODO: no unwrap
        // TODO: match parens at lex level
        let t = c.tokens.peek().unwrap();
        let infix = match t.kind {
            k if k == end => break,
            TokenKind::LBrak => {
                c.tokens.next();
                let rhs = parse_expr(c, 0., TokenKind::RBrak);
                c.tokens.next();
                lhs = Expr::Infix(Infix::Index, (lhs, rhs).boxed());
                continue;
            }
            TokenKind::LPar => {
                c.tokens.next();
                let rhs = parse_expr(c, 0., TokenKind::RPar);
                c.tokens.next();
                lhs = Expr::Infix(Infix::FunCall, (lhs, rhs).boxed());
                continue;
            }
            _ if try_parse_postfix(&t, c).is_some() => {
                // TODO: no unwrap
                let postfix = try_parse_postfix(&t, c).unwrap();
                c.tokens.next();
                lhs = Expr::Postfix(postfix, lhs.boxed());
                continue;
            }
            _ => parse_infix(t, c),
        };
        let (l_bp, r_bp) = infix_bp(infix);
        if l_bp < bp {
            break;
        }
        c.tokens.next();
        let rhs = parse_expr(c, r_bp, end);
        lhs = Expr::Infix(infix, (lhs, rhs).boxed());
    }

    lhs
}

struct Compiler {
    tokens: Iter<Token>,
    source: String,
}

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = "main.bp";
    let source = read_to_string(input_path)?;
    let tokens = tokenize(&source.trim_end(), input_path);

    for token in &tokens {
        println!("{}", token.loc_string());
    }

    let mut compiler = Compiler {
        tokens: Iter::new(tokens),
        source,
    };

    let expr = parse_expr(&mut compiler, 0., TokenKind::Eof);
    println!("{}", expr.string());

    Ok(())
}
