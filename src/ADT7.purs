module ADT7 where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over2, wrap)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)

class Expr e where
  lit :: Number -> e Number
  add_ :: e Number -> e Number -> e Number 

newtype Eval a = Eval a

derive instance genericEval :: Generic (Eval a) _
derive instance newtypeEval :: Newtype (Eval a) _

instance showEval :: Show a => Show (Eval a) where
  show x = genericShow x

instance evalExpr :: Expr Eval where
  lit = wrap
  add_ = over2 Eval (+)

expr :: forall e. Expr e => e Number
expr = add_ (lit 10.0) (lit 20.0)

class Expr e <= AdvExpr e where
  mul_ :: e Number -> e Number -> e Number

instance advExprEval :: AdvExpr Eval where
  mul_ = over2 Eval (*)

expr' :: forall e. AdvExpr e => e Number
expr' = mul_ expr expr

class AdvExpr2 e where
  mul2_ :: e Number -> e Number -> e Number

instance advExpr2Eval :: AdvExpr2 Eval where
  mul2_ = over2 Eval (*)

expr2 :: forall e. Expr e => AdvExpr2 e => e Number
expr2 = mul2_ expr expr

main :: Effect Unit
main = do
  log "---- ADT7 ----"
  logShow $ expr :: Eval Number
  logShow $ expr' :: Eval Number
  logShow $ expr2 :: Eval Number
  log "--------------"