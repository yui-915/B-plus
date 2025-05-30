import { tokenize } from "./lexer.ts";
import { parse } from "./parser.ts";

const input_path = "main.bp";
const source = Deno.readTextFileSync(input_path);

const tokens = tokenize(source, input_path);
console.log(tokens.map((e) => e.array()));

const ast = parse(tokens);
console.log(ast);
