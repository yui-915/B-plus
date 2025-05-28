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
    #[token(":=")]
    ColonEq,
    #[token("=")]
    Eq,

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
    VarDef(VarDef),
    VarAssign(VarAssign),
    Block(Vec<Statement>),
}

#[derive(Debug)]
struct VarDef {
    name: String,
    value: RValue,
}

#[derive(Debug)]
struct VarAssign {
    name: String,
    value: RValue,
}

#[derive(Debug)]
struct FunCall {
    name: String,
    args: Vec<RValue>,
}

#[derive(Debug)]
enum RValue {
    Variable(String),
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

    while c.tok_idx < c.tokens.len() {
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
    } else if let Ok(var_def) = parse_var_def(c) {
        Statement::VarDef(var_def)
    } else if let Ok(var_assign) = parse_var_assign(c) {
        Statement::VarAssign(var_assign)
    } else {
        Statement::Block(parse_block(c)?)
    };
    c.expect(Token::Semi)?;
    Ok(result)
}

fn parse_var_assign(c: &mut Compiler) -> Result<VarAssign> {
    let ident = c.expect_off(Token::Ident, 0)?;
    c.expect_off(Token::Eq, 1)?;
    c.tok_idx += 2;

    let name = c.source[ident.1].to_owned();
    let value = parse_rvalue(c)?;

    Ok(VarAssign { name, value })
}

fn parse_var_def(c: &mut Compiler) -> Result<VarDef> {
    let ident = c.expect_off(Token::Ident, 0)?;
    c.expect_off(Token::ColonEq, 1)?;
    c.tok_idx += 2;

    let name = c.source[ident.1].to_owned();
    let value = parse_rvalue(c)?;

    Ok(VarDef { name, value })
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
    let ident = c.expect_off(Token::Ident, 0)?;
    c.expect_off(Token::LPar, 1)?;
    c.tok_idx += 2;

    let name = c.source[ident.1].to_owned();
    let mut args = vec![];

    while let Ok(arg) = parse_rvalue(c) {
        args.push(arg);
    }
    c.expect(Token::RPar)?;

    Ok(FunCall { name, args })
}

fn parse_rvalue(c: &mut Compiler) -> Result<RValue> {
    let result = if let Ok(constant) = parse_constant(c) {
        RValue::Constant(constant)
    } else {
        RValue::Variable(parse_ident(c)?)
    };
    Ok(result)
}

fn parse_ident(c: &mut Compiler) -> Result<String> {
    let ident = c.expect(Token::Ident)?;
    let name = c.source[ident.1].to_owned();
    Ok(name)
}

fn parse_constant(c: &mut Compiler) -> Result<Constant> {
    let (_, span) = c.expect(Token::Integer)?;
    let int = c.source[span].parse()?;
    Ok(Constant::Integer(int))
}

struct Compiler {
    input_path: PathBuf,
    source: String,
    tokens: Vec<(Result<Token, ()>, Range<usize>)>,
    tok_idx: usize,
}

impl Compiler {
    #[must_use]
    fn expect(&mut self, tok: Token) -> Result<(Token, Range<usize>)> {
        let res = self.expect_off(tok, 0)?;
        self.tok_idx += 1;
        Ok(res)
    }
    fn expect_off(&mut self, tok: Token, offset: usize) -> Result<(Token, Range<usize>)> {
        let idx = self.tok_idx + offset;
        if idx >= self.tokens.len() {
            bail!("expected `{:?}` but found `EOF`", tok);
        }
        let (token, span) = self.tokens[idx].clone();
        let Ok(token) = token else {
            bail!("expected `{:?}` but found `ERROR` as `{:?}`", tok, span);
        };
        if token != tok {
            bail!("expected `{:?}` but found `{:?}` at {:?}", tok, token, span);
        }
        Ok((token, span))
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
        Statement::VarDef(var_def) => {
            out.push_("WORD");
            out.push(var_def.name);
            out.push_("=");
            generate_rvalue(out, var_def.value);
        }
        Statement::VarAssign(var_assign) => {
            out.push(var_assign.name);
            out.push_("=");
            generate_rvalue(out, var_assign.value);
        }
    }
    out.push_(";");
}

fn generate_rvalue(out: &mut Vec<String>, val: RValue) {
    match val {
        RValue::Constant(con) => generate_constant(out, con),
        RValue::Variable(name) => out.push(name),
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
        tokens: Token::lexer(&source).spanned().collect(),
        tok_idx: 0,
    };
    let file = parse_file(&mut c)?;
    let output = generate_output(file);
    println!("{output}");

    Ok(())
}
