export function panic(...msg: unknown[]): never {
  console.error(...msg);
  throw "PANIC";
}
