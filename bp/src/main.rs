#![allow(unused, unused_variables, dead_code)]
use {
    anyhow::{Result, bail},
    clap::Parser,
    logos::{Logos, Span, SpannedIter},
    std::{fs::read_to_string, iter::Peekable, ops::Range, path::PathBuf},
};

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \r\n\t]+")]
enum Token {
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token("(")]
    LPar,
    #[token(")")]
    RPar,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,

    #[token("extern")]
    Extern,

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*")]
    Ident,
    #[regex(r"[0-9]+")]
    Integer,
}

#[derive(Parser, Debug)]
struct Cli {
    file: PathBuf,
}

#[derive(Debug)]
struct File {
    externs: Vec<String>,
    funcs: Vec<Func>,
}

#[derive(Debug)]
struct Func {
    name: String,
    body: Statement,
}

#[derive(Debug)]
enum Statement {
    FunCall(FunCall),
    Block(Vec<Statement>),
}

#[derive(Debug)]
struct FunCall {
    name: String,
    args: Vec<RValue>,
}

#[derive(Debug)]
enum RValue {
    Constant(Constant),
}

#[derive(Debug)]
enum Constant {
    Integer(u64),
}

fn parse_file(c: &mut Compiler) -> Result<File> {
    let mut file = File {
        externs: vec![],
        funcs: vec![],
    };

    while c.lexer.peek().is_some() {
        if let Ok(func) = parse_func(c) {
            file.funcs.push(func);
        } else {
            file.externs.push(parse_extern(c)?);
        }
    }

    Ok(file)
}
fn parse_extern(c: &mut Compiler) -> Result<String> {
    c.expect(Token::Extern)?;
    let ident = c.expect(Token::Ident)?;
    let name = c.source[ident.1].to_owned();
    c.expect(Token::Semi)?;

    Ok(name)
}

fn parse_func(c: &mut Compiler) -> Result<Func> {
    let ident = c.expect(Token::Ident)?;
    let name = c.source[ident.1].to_owned();
    c.expect(Token::LPar)?;
    c.expect(Token::RPar)?;
    let body = parse_statement(c)?;
    Ok(Func { name, body })
}

fn parse_statement(c: &mut Compiler) -> Result<Statement> {
    let result = if let Ok(funcall) = parse_funcall(c) {
        Statement::FunCall(funcall)
    } else {
        Statement::Block(parse_block(c)?)
    };
    c.expect(Token::Semi)?;
    Ok(result)
}

fn parse_block(c: &mut Compiler) -> Result<Vec<Statement>> {
    let mut stmts = vec![];

    c.expect(Token::LBrace)?;
    while let Ok(stmt) = parse_statement(c) {
        stmts.push(stmt);
    }
    c.expect(Token::RBrace)?;

    Ok(stmts)
}

fn parse_funcall(c: &mut Compiler) -> Result<FunCall> {
    let ident = c.expect(Token::Ident)?;
    let name = c.source[ident.1].to_owned();
    let mut args = vec![];

    c.expect(Token::LPar)?;
    while let Ok(arg) = parse_rvalue(c) {
        args.push(arg);
    }
    c.expect(Token::RPar)?;

    Ok(FunCall { name, args })
}

fn parse_rvalue(c: &mut Compiler) -> Result<RValue> {
    Ok(RValue::Constant(parse_constant(c)?))
}

fn parse_constant(c: &mut Compiler) -> Result<Constant> {
    let (_, span) = c.expect(Token::Integer)?;
    let int = c.source[span].parse()?;
    Ok(Constant::Integer(int))
}

struct Compiler<'a> {
    input_path: PathBuf,
    source: String,
    lexer: Peekable<SpannedIter<'a, Token>>,
}

impl<'a> Compiler<'a> {
    #[must_use]
    fn expect(&mut self, tok: Token) -> Result<(Token, Range<usize>)> {
        let Some((_, span)) = self.lexer.peek() else {
            bail!("expected `{:?}` but found `EOF`", tok);
        };
        let span = span.clone();
        let ((Ok(token), _)) = self.lexer.peek().unwrap().clone() else {
            bail!("expected `{:?}` but found `ERROR` as `{:?}`", tok, span);
        };
        if token != tok {
            bail!("expected `{:?}` but found `{:?}` at {:?}", tok, token, span);
        }
        self.lexer.next();
        Ok((token.clone(), span))
    }
}

trait VecExt {
    fn push_(&mut self, s: &str);
}
impl VecExt for Vec<String> {
    fn push_(&mut self, s: &str) {
        self.push(s.to_owned())
    }
}

fn generate_output(file: File) -> String {
    let mut out = vec![];

    out.push_("typedef long long WORD;");

    for name in file.externs {
        out.push(format!("WORD {name}(...);"))
    }

    for func in file.funcs {
        out.push(format!("WORD {}() {{", func.name));
        generate_statement(&mut out, func.body);
        out.push_("return 0;}");
    }

    out.join("\n")
}

fn generate_statement(out: &mut Vec<String>, stmt: Statement) {
    match stmt {
        Statement::FunCall(funcall) => {
            out.push(funcall.name);
            out.push_("(");
            for arg in funcall.args {
                generate_rvalue(out, arg);
            }
            out.push_(")");
        }
        Statement::Block(stmts) => {
            out.push_("{");
            for stmt in stmts {
                generate_statement(out, stmt);
            }
            out.push_("}");
        }
    }
    out.push_(";");
}

fn generate_rvalue(out: &mut Vec<String>, val: RValue) {
    match val {
        RValue::Constant(con) => generate_constant(out, con),
    }
}

fn generate_constant(out: &mut Vec<String>, con: Constant) {
    match con {
        Constant::Integer(int) => out.push(int.to_string()),
    }
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let source = read_to_string(&cli.file)?;
    let mut c = Compiler {
        input_path: cli.file,
        source: source.clone(),
        lexer: Token::lexer(&source).spanned().peekable(),
    };
    let file = parse_file(&mut c)?;
    let output = generate_output(file);
    println!("{output}");

    Ok(())
}
