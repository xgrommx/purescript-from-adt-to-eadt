module ADT3 where

import Prelude

import Data.Functor.Mu (Mu(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Matryoshka (cata)

data ExprF a
  = LitF Number
  | AddF a a

derive instance functorExprF :: Functor ExprF

type Expr = Mu ExprF

-- Smart constructors

lit :: Number -> Expr
lit a = In (LitF a)

add :: Expr -> Expr -> Expr
add a b = In (AddF a b)

eval :: Expr -> Number
eval = cata evalAlgebra where
  evalAlgebra = case _ of
    LitF a -> a
    AddF a b -> a + b

expr :: Expr
expr = add (lit 10.0) (lit 20.0)

main :: Effect Unit
main = do
  log "---- ADT3 ----"
  logShow $ eval expr
  log "--------------"