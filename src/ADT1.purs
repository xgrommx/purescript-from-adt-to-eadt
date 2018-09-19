module ADT1 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)

data Expr
  = Lit Number
  | Add Expr Expr

derive instance genericExpr :: Generic Expr _

instance eqExpr :: Eq Expr where
  eq x y = genericEq x y

instance showExpr :: Show Expr where
  show x = genericShow x

eval :: Expr -> Number
eval = case _ of
  Lit x -> x
  Add x y -> eval x + eval y

expr :: Expr
expr = Add (Lit 10.0) (Lit 20.0)

-- But what if we want add a new constructor like Mul? We should modify original file. This approach is not extensible :(

main :: Effect Unit
main = do
  log "---- ADT1 ----"
  logShow expr
  logShow $ eval expr
  log "--------------"