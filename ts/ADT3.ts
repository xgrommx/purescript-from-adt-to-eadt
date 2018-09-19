namespace ADT3 {
  class Eval {
    public eval: () => number;
  
    constructor(val: number) {
      this.eval = () => val;
    }
  }
  
  class View {
    public print: () => string;
  
    constructor(val: string) {
      this.print = () => val;
    }
  }
  
  interface ExpAlg<T> {
    lit(n: number): T;
    add(x: T, y: T): T;
  }
  
  class ViewExp implements ExpAlg<View> {
    static create() {
      return new ViewExp();
    }
  
    lit(n: number): View {
      return new View(n.toString())
    }
  
    add(x: View, y: View): View {
      return new View(`(${x.print()} + ${y.print()})`);
    }
  }
  
  class EvalExp implements ExpAlg<Eval> {
    static create() {
      return new EvalExp();
    }
  
    lit(n: number): Eval {
      return new Eval(n);
    }
  
    add(x: Eval, y: Eval): Eval {
      return new Eval(x.eval() + y.eval());
    }
  }
  
  function expr <T> (f: ExpAlg<T>): T {
      return f.add(
          f.lit(1),
          f.add(
              f.lit(2),
              f.lit(3)));
  }
  
  const v1: number = expr(EvalExp.create()).eval();
  const s1: string = expr(ViewExp.create()).print();
  
  console.log(v1);
  console.log(s1);
}