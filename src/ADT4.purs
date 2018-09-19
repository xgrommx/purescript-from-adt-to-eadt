module ADT4 where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Matryoshka (class Recursive, cata)

data Expr
  = Lit Number
  | Add Expr Expr

data ExprF a
  = LitF Number
  | AddF a a

derive instance functorExprF :: Functor ExprF

instance recursiveExpr :: Recursive Expr ExprF where
  project = case _ of
    Lit a -> LitF a
    Add a b -> AddF a b

eval :: Expr -> Number
eval = cata evalAlgebra where
  evalAlgebra = case _ of
    LitF a -> a
    AddF a b -> a + b

expr :: Expr
expr = Add (Lit 10.0) (Lit 20.0)

main :: Effect Unit
main = do
  log "---- ADT4 ----"
  logShow $ eval expr
  log "--------------"