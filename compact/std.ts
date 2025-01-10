export type Uint<N extends number = 64> = number;
export type Bytes<N extends number = 64> = string;

export class Counter {
  value: bigint;

  constructor(initialValue = 0) {
    this.value = BigInt(initialValue);
  }

  increment(val: number) {
    this.value = BigInt(Number(this.value) + val);
  }

  decrement(val: number) {
    this.value = BigInt(Number(this.value) - val);
  }
}

export class Cell<T> {
  value: T;

  constructor(initialValue: T) {
    this.value = initialValue;
  }

  read(): T {
    return this.value;
  }

  write(newValue: T) {
    this.value = newValue;
  }
}

export abstract class CompactContract<LedgerType> {
  abstract ledger: LedgerType;
}
