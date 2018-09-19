namespace ADT2 {
  interface Eval {
    eval(): number; 
  }
  
  interface ExpAlg<T> {
    lit(n: number): T;
    add(x: T, y: T): T;
  }
  
  class EvalExp implements ExpAlg<Eval> {
    lit(n: number): Eval {
      return {
        eval() {
          return n
        }
      }
    }
  
    add(x: Eval, y: Eval): Eval {
      return {
        eval() {
          return x.eval() + y.eval();
        }
      }
    }
  }
  
  function expr <T> (f: ExpAlg<T>): T {
      return f.add(
          f.lit(1),
          f.add(
              f.lit(2),
              f.lit(3)));
  }
  
  const v1: number = expr(new EvalExp()).eval();
  
  console.log(v1);
  
  interface MulAlg<T> extends ExpAlg<T> {
    mul(x: T, y: T): T;
  }
  
  class EvalExpExt extends EvalExp implements MulAlg<Eval> {
    mul(x: Eval, y: Eval): Eval {
      return {
        eval() {
          return x.eval() + y.eval();
        }
      }
    }
  }
  
  function expr2<T>(f: MulAlg<T>): T {
    return f.mul(
      f.lit(4),
      f.add(
        f.lit(5),
        f.lit(6)
      )
    )
  }
  
  const v2: number = expr2(new EvalExpExt()).eval();
  
  console.log(v2);
  
  interface View {
    view(): string;
  }
  
  class ViewExp implements ExpAlg<View> {
    lit(n: number): View{
        return {
          view() {
            return n.toString()
          }
        }
    }
  
    add(x: View, y: View): View {
      return {
        view() {
          return `(${x.view()} + ${y.view()})`
        }
      }
    }
  }
  
  class ViewMul extends ViewExp implements MulAlg<View> {
      mul(x: View, y: View): View {
        return {
          view() {
            return `(${x.view()} * ${y.view()})`;
          }
        }
      }
  }
  
  const s1: string = expr(new ViewExp()).view();
  const s2: string = expr2(new ViewMul()).view();
  
  console.log(s1);
  console.log(s2);
}