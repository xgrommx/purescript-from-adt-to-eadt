module ADT2 where

import Prelude

import Data.Either (Either(..), either)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)

-- Instead of type sum we can define our constructors separately

data Lit = Lit Number
data Add a b = Add a b

-- How we can combine our constructors? Type classes rescue!

class Expr e

instance exprLit :: Expr Lit
instance exprAdd :: (Expr a, Expr b) => Expr (Add a b)

class Expr e <= Eval e where
  eval :: e -> Number

instance evalLit :: Eval Lit where
  eval (Lit n) = n

instance evalAdd :: (Eval a, Eval b) => Eval (Add a b) where
  eval (Add a b) = eval a + eval b

-- Okay! We can add a new constructor without modification original file. Also we can add a new method.

expr :: Add Lit Lit
expr = Add (Lit 10.0) (Lit 20.0)

-- expr2 x = if x > 10.0 then Lit x else Add (Lit x) (Lit x) -- But here we have problem!

expr2 :: Number -> Either Lit (Add Lit Lit) -- Type sum rescue!
expr2 x = if x > 10.0 then Left (Lit x) else Right (Add (Lit x) (Lit x))

main :: Effect Unit
main = do
  log "---- ADT2 ----"
  logShow $ eval expr -- We can still evaluate our expression
  logShow $ either eval eval (expr2 100.0)
  log "--------------"