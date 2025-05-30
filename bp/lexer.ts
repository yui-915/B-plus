import { Iter } from "./iter.ts";
import { panic } from "./util.ts";

export enum Symbol {
  Plus = "+",
  Star = "*",
  LPar = "(",
  RPar = ")",
  // LBrace = "{",
  // RBrace = "}",
  // ColonEq = ":=",
  // EqEq = "==",
  // Semi = ";",
}

export enum Keyword {
  // if = "if",
}

export type TokenType = Symbol | Keyword | "number" | "ident";

export function where(input_path: string, line: number, offset: number) {
  return `${input_path}:${line}:${offset}`;
}

export class Token {
  type: TokenType;
  value: string;
  line: number;
  offset: number;
  length: number;
  input_path: string;

  constructor(
    type: TokenType,
    value: string,
    line: number,
    offset: number,
    length: number,
    input_path: string,
  ) {
    this.type = type;
    this.value = value;
    this.line = line;
    this.offset = offset;
    this.length = length;
    this.input_path = input_path;
  }

  array() {
    const pos = where(this.input_path, this.line, this.offset);
    return this.type == this.value
      ? [this.type, pos]
      : [this.type, this.value, pos];
  }
}

export function tokenize(source: string, input_path: string): Token[] {
  const i = new Iter(source.split(""));
  const tokens: Token[] = [];

  let line = 1, offset = 0, start = 0;
  const push = (type: TokenType, value: string) =>
    tokens.push(
      new Token(type, value, line, offset, start - i.idx + 1, input_path),
    );

  while (!i.empty()) {
    start = i.idx;
    offset++;
    const c = i.next();
    if (c == "\n") {
      line++;
      offset = 0;
    }
    if (is_space(c)) continue;

    let buf = c;
    let pairs = Object.entries(Symbol).filter(([_k, v]) => v.startsWith(buf));
    while (pairs.length > 1) {
      buf += i.next();
      pairs = pairs.filter(([_k, v]) => v.startsWith(buf));
    }
    if (pairs.length == 1) {
      push(pairs[0][1], pairs[0][1]);
      continue;
    } else i.back(buf.length - 1);

    if (is_num(c)) {
      i.back();
      push("number", i.take_while(is_num).join(""));
      continue;
    }

    if (is_alpha(c)) {
      i.back();
      const value = i.take_while(is_alphanum).join("");
      const pair = Object.entries(Keyword).find(([_k, v]) => v == c);
      if (pair) push(pair[1], value);
      else push("ident", value);
      continue;
    }

    const pos = where(input_path, line, offset);
    panic(`${pos}: Lexer error: unknown char \`${c}\``);
  }

  return tokens;
}

function is_num(c: string) {
  return "0123456789".includes(c);
}

function is_alpha(c: string) {
  let a = "abcdefghijklmnopqrstuvwxyz";
  a += a.toUpperCase();
  return a.includes(c);
}

function is_alphanum(c: string) {
  return is_num(c) || is_alpha(c);
}

function is_space(c: string) {
  return " \t\r\n".includes(c);
}
