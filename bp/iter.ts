import { panic } from "./util.ts";

export class Iter<T> {
  items: T[];
  idx = 0;
  stack: number[] = [];

  constructor(items: T[]) {
    this.items = items;
  }

  peek(offset = 0) {
    return this.items[this.idx + offset];
  }
  next() {
    return this.items[this.idx++];
  }
  take_while(pred: (e: T) => boolean): T[] {
    const result = [];
    while (!this.empty() && pred(this.peek())) result.push(this.next());
    return result;
  }
  back(amount = 1) {
    this.idx -= amount;
  }
  empty() {
    return this.peek() == null;
  }

  save() {
    this.stack.push(this.idx);
  }
  restore() {
    this.idx = this.stack.pop() ?? panic("iter stack underflow");
  }
  commit() {
    this.stack.pop();
  }
}
