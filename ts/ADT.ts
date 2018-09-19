namespace ADT {
  interface Expr {
    eval(): number;
  }
  
  interface Print {
    print(): string;
  }
  
  class Lit implements Expr, Print {
    private readonly v: number;
  
    constructor(v: number) {
      this.v = v;
    }
  
    eval(): number {
      return this.v;
    }
  
    print(): string {
      return this.v.toString();
    }
  }
  
  class Add implements Expr, Print {
    private readonly l;
    private readonly r;
  
    constructor(l: Expr, r: Expr) {
      this.l = l;
      this.r = r;
    }
  
    eval(): number {
      return this.l.eval() + this.r.eval();
    }
  
    print(): string {
      return `(${this.l.print()} + ${this.r.print()})`;
    }
  }
  
  class Mul implements Expr {
    private readonly l;
    private readonly r;
  
    constructor(l: Expr, r: Expr) {
      this.l = l;
      this.r = r;
    }
  
    eval(): number {
      return this.l.eval() * this.r.eval();
    }
  }
}