import { Iter } from "./iter.ts";
import { Symbol, Token, TokenType, where } from "./lexer.ts";
import { panic } from "./util.ts";

// const prefix_bp = {};

const infix_bp: Map<TokenType[], [number, number]> = new Map();
infix_bp.set([Symbol.Plus], [1, 1.1]);
infix_bp.set([Symbol.Star], [2, 2.1]);

function is_infix(t: TokenType) {
  return infix_bp.keys().some((e) => e.includes(t));
}

function get_infex_bp(t: TokenType) {
  return infix_bp.entries().find(([k, _v]) => k.includes(t))?.[1] as [
    number,
    number,
  ];
}

export interface Func {
  name: string;
}

export interface Ast {
  funcs: Func[];
}

type Expr = Atom | Infix;

interface Atom {
  type: "atom";
  token: Token;
}

enum InfixOp {
  Add = "+",
}

interface Infix {
  type: "infix";
  op: InfixOp;
  lhs: Expr;
  rhs: Expr;
}

export function parse(tokens: Token[]) {
  const i = new Iter(tokens);
  return parse_expr(i, 0);
}

function expect(i: Iter<Token>, type: TokenType) {
  const t = i.next();
  const pos = where(t.input_path, t.line, t.offset);
  if (t.type != type) {
    panic(`${pos}: Parser error: expected \`${type}\` but found \`${t.type}\``);
  }
}

function parse_expr(i: Iter<Token>, min_bp: number): Expr {
  let token: Token, lhs: Expr, op;

  token = i.next();
  if (is_atom(token)) lhs = { type: "atom", token };
  else if (token.type == Symbol.LPar) {
    lhs = parse_expr(i, 0);
    expect(i, Symbol.RPar);
    // } else if (is_prefix(token.type)) {
    //   op = token.type;
    //   const r_bp = prefix_binding_power(op);
    //   const rhs = parse_expr(i, r_bp);
    //   lhs = { type: "prefix", op, rhs };
    //   return lhs;
  } else panic("bad token [0]:", token);

  while (true) {
    token = i.peek();
    if (token == null) break;
    else if (token.type == ")") break;
    else if (is_infix(token.type)) op = token.type;
    else panic("bad token [1]:", token);

    let [l_bp, r_bp] = get_infex_bp(op);
    if (l_bp < min_bp) {
      break;
    }
    i.next();
    let rhs = parse_expr(i, r_bp);

    lhs = { type: "infix", op: op as InfixOp, lhs, rhs };
  }

  return lhs;
}

function is_atom(token: Token) {
  return token.type == "ident" || token.type == "number";
}
