// <compiler-version 0.20.0>
// -> compiler-version is not mandatory, but must be provided under this format

import { Counter, Uint } from "../std";
// imports from STD will be ignored by the compiler
// imports from other modules will be included in the output

class Ledger {
  // MUST: the contract must include a class named Ledger
  round: Counter;
  // MUST: properties of the Ledger class must be ledger types

  constructor() {
    // MUST: the Ledger class must have a constructor to initialize the ledger
    this.round = new Counter();
  }
}

export class Contract {
  // MUST: the contract must include a class named Contract
  ledger: Ledger;
  // MUST: there must be a ledger property in the contract

  constructor() {
    this.ledger = new Ledger();
  }

  public increment(val: Uint<32>) {
    this.ledger.round.increment(val);
  }
}
