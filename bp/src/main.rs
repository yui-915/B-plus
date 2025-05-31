#![allow(dead_code)]

use std::{
    borrow::Borrow,
    error::Error,
    fmt::{self, Display, Formatter},
    fs::read_to_string,
};

#[cfg(test)]
mod tests;

macro_rules! diag {
    ($token:expr, $($fmt:tt)*) => {
        eprint!("{}", $token.loc());
        eprintln!($($fmt)*);
    };
}

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
        other    {$( $other:ident   $other_str:literal   )*}
    } => {
        #[derive(PartialEq, Clone, Debug, Copy)]
        pub enum TokenKind {
            $($symbol,)*
            $($keyword,)*
            $($other,)*
        }

        impl Display for TokenKind {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let s = match self {
                    $(TokenKind::$symbol => $symbol_str,)*
                    $(TokenKind::$keyword => $keyword_str,)*
                    $(TokenKind::$other => $other_str,)*
                };
                write!(f, "`{s}`")
            }
        }

        impl TokenKind {
            fn to_raw_string(&self) -> String {
                match self {
                    $(TokenKind::$symbol => $symbol_str,)*
                    $(TokenKind::$keyword => $keyword_str,)*
                    $(TokenKind::$other => $other_str,)*
                }.to_owned()
            }
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
        Ident   "identifier"
        Integer "integer literal"
        // Float   "float literal"
        // String  "string literal"
        // Char    "character literal"
        Error   "ERROR WTF THIS IS LEXER ERROR PLEASE REPORT"
        Eof     "End of file"
    }
}

macro_rules! gen_parser_helpers {
    {
        infix   {$( $infix_token_kind:ident   => $infix:ident,   )*}
        prefix  {$( $prefix_token_kind:ident  => $prefix:ident,  )*}
        postfix {$( $postfix_token_kind:ident => $postfix:ident, )*}
    } => {
        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Infix {
            $($infix,)*
        }

        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Prefix {
            $($prefix,)*
        }

        #[derive(Clone, Copy, Debug, PartialEq)]
        enum Postfix {
            $($postfix,)*
        }

        impl Display for Infix {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let t = match self {
                    $(Infix::$infix => TokenKind::$infix_token_kind,)*
                };
                write!(f, "{}", t.to_raw_string())
            }
        }

        impl Display for Prefix {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let t = match self {
                    $(Prefix::$prefix => TokenKind::$prefix_token_kind,)*
                };
                write!(f, "{}", t.to_raw_string())
            }
        }

        impl Display for Postfix {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                let t = match self {
                    $(Postfix::$postfix => TokenKind::$postfix_token_kind,)*
                };
                write!(f, "{}", t.to_raw_string())
            }
        }

        fn parse_infix(t: Token, _c: &mut Compiler) -> Infix {
            match t.kind {
                $(TokenKind::$infix_token_kind => Infix::$infix,)*
                _ => {
                    diag!(
                        t,
                        "Unexpected token {} while trying to parse infix",
                        t.kind
                    );
                    panic!();
                }
            }
        }

        fn try_parse_prefix<T: Borrow<Token>>(t: T, _c: &mut Compiler) -> Option<Prefix> {
            match t.borrow().kind {
                $(TokenKind::$prefix_token_kind => Some(Prefix::$prefix),)*
                _ => None,
            }
        }


        fn try_parse_postfix<T: Borrow<Token>>(t: T, _c: &mut Compiler) -> Option<Postfix> {
            match t.borrow().kind {
                $(TokenKind::$postfix_token_kind => Some(Postfix::$postfix),)*
                _ => None,
            }
        }
    };
}

gen_parser_helpers! {
    infix {
        Plus  => Add,
        Star  => Mul,
        Minus => Sub,
    }
    prefix {
        Minus    => Negate,
        PlusPlus => Increment,
    }
    postfix {
        PlusPlus => Increment,
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

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Integer(int) => write!(f, "{int}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Atom(value) => write!(f, "{value}"),
            Expr::Infix(infix, operands) => write!(f, "({} {} {})", infix, operands.0, operands.1,),
            Expr::Prefix(prefix, operand) => write!(f, "({} {})", prefix, operand,),
            Expr::Postfix(postfix, operand) => write!(f, "({} {})", postfix, operand,),
            Expr::Index(operands) => write!(f, "([ {} {})", operands.0, operands.1),
            Expr::FunCall(func, args) => {
                write!(
                    f,
                    "(f {} {})",
                    func,
                    args.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
        }
    }
}

enum Expr {
    Atom(Value),
    Infix(Infix, Box<(Expr, Expr)>),
    Prefix(Prefix, Box<Expr>),
    Postfix(Postfix, Box<Expr>),
    Index(Box<(Expr, Expr)>),
    FunCall(Box<Expr>, Vec<Expr>),
}

enum Value {
    Integer(u64),
}

fn parse_value(t: Token, c: &mut Compiler) -> Expr {
    // TODO: ident

    let value = match t.kind {
        TokenKind::Integer => match t.str(c).parse() {
            Ok(int) => Value::Integer(int),
            Err(err) => {
                diag!(t, "Error while parsing number: {err}");
                panic!();
            }
        },
        _ => {
            diag!(t, "Unexpected token {} while trying to parse value", t.kind);
            panic!();
        }
    };

    Expr::Atom(value)
}

// TODO: automate using macro
fn infix_bp(i: Infix) -> (f32, f32) {
    use Infix::*;
    match i {
        Add | Sub => (1., 1.1),
        Mul => (2., 2.1),
    }
}
const PREFIX_BP: f32 = 3.;

fn parse_expr(c: &mut Compiler, bp: f32, end: &[TokenKind]) -> Expr {
    let t = c.tokens.next();
    let mut lhs;

    if t.kind == TokenKind::LPar {
        lhs = parse_expr(c, 0., &[TokenKind::RPar]);
        c.tokens.next();
    } else if let Some(prefix) = try_parse_prefix(&t, c) {
        let operand = parse_expr(c, PREFIX_BP, end);
        lhs = Expr::Prefix(prefix, operand.boxed());
    } else {
        lhs = parse_value(t, c);
    }

    loop {
        // TODO: match parens at lex level
        let t = c.tokens.peek().unwrap();
        if end.iter().any(|&e| e == t.kind) {
            break;
        } else if t.kind == TokenKind::LBrak {
            c.tokens.next();
            let rhs = parse_expr(c, 0., &[TokenKind::RBrak]);
            c.tokens.next();
            lhs = Expr::Index((lhs, rhs).boxed());
        } else if t.kind == TokenKind::LPar {
            c.tokens.next();
            let mut args = vec![];
            loop {
                args.push(parse_expr(c, 0., &[TokenKind::RPar, TokenKind::Comma]));
                if c.tokens.next().kind == TokenKind::RPar {
                    break;
                }
            }
            lhs = Expr::FunCall(lhs.boxed(), args);
        } else if let Some(postfix) = try_parse_postfix(&t, c) {
            c.tokens.next();
            lhs = Expr::Postfix(postfix, lhs.boxed());
        } else {
            let infix = parse_infix(t, c);
            let (l_bp, r_bp) = infix_bp(infix);
            if l_bp < bp {
                break;
            }
            c.tokens.next();
            let rhs = parse_expr(c, r_bp, end);
            lhs = Expr::Infix(infix, (lhs, rhs).boxed());
        }
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

    let expr = parse_expr(&mut compiler, 0., &[TokenKind::Eof]);
    println!("{}", expr);

    Ok(())
}
