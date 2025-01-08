// <compiler-version 0.20.0>

import { Counter, Cell, Uint, Bytes, CompactContract } from "../std";

class Ledger {
  round: Counter;
  test: Cell<Bytes<32>>;
  uint_number: Cell<Uint<64>>;
  ts_number: Cell<number>;

  constructor() {
    this.round = new Counter();
    this.test = new Cell("2c45f89bb");
    this.uint_number = new Cell(420);
    this.ts_number = new Cell(123);
  }
}

class Contract extends CompactContract<Ledger> {
  ledger: Ledger;

  constructor() {
    super();
    this.ledger = new Ledger();
  }

  public increment(val: number): void {
    this.ledger.round.increment(val);
  }
}
