module ADT8 where

import Prelude

import Control.Monad.Free (Free, resume, wrap)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)

-- TODO: https://github.com/natefaubion/purescript-run

-- Here we don't need `LitF` because we have `pure`

data ExprF a = AddF a a

derive instance functorExprF :: Functor ExprF

type Expr = Free ExprF

-- Smart Constructors

lit :: forall a. a -> Expr a
lit n = pure n

add :: forall a. Expr a -> Expr a -> Expr a
add a b = wrap (AddF a b)

iter :: forall f a. Functor f => (f a -> a) -> Free f a -> a
iter k = go where
  go m = case resume m of
    Left f -> k (go <$> f)
    Right a -> a

expr :: Expr Number
expr = add (lit 10.0) (lit 20.0)

eval :: Expr Number -> Number
eval = iter evalAlgebra
  where
    evalAlgebra (AddF a b) = a + b

main :: Effect Unit
main = do
  log "---- ADT8 ----"
  logShow $ eval expr
  log "--------------"