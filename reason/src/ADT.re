type eval = {eval: int};
type view = {view: string};

module type Exp = {
  type t; 
  let lit: int => t; 
  let add: (t, t) => t;
};

module ExpEval = {
  type t = eval;
  let lit = (n) => {
    eval: n
  };
  let add = (x, y) => {
    eval: x.eval + y.eval
  };
};

module ExpView = {
  type t = view;
  let lit = (n) => {view: string_of_int(n)};
  let add = (x, y) => {
    view: "(" ++ x.view ++ " + " ++ y.view ++ ")"
  };
};

module type Mul = {
  include Exp; 
  let mul: (t, t) => t;
};

module MulEval = {
  include ExpEval;
  let mul = (x, y) => {
    eval: x.eval * y.eval
  };
};

module MulView = {
  include ExpView;
  let mul = (x, y) => {
    view: "(" ++ x.view ++ " * " ++ y.view ++ ")"
  };
};

let expr = (type s, (module M): (module Exp with type t = s)) => M.add(M.add(M.lit(10), M.lit(12)), M.lit(40));

let v1 = expr(module ExpEval);
let s1 = expr(module ExpView);

let expr2 = (type s, (module M): (module Mul with type t = s)) => M.mul(M.add(M.lit(10), M.lit(12)), M.lit(40));

let v2 = expr2(module MulEval);
let s2 = expr2(module MulView);