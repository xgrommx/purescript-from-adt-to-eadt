module ADT9 where

import Prelude hiding (add)

import Data.Either (Either(..))
import Data.Functor.Variant (VariantF, FProxy, on, case_)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (logShow)
import Run (Run, lift, peel)
import Type.Row (type (+))

data AddF a = AddF a a

derive instance functorAddF :: Functor AddF

_add :: SProxy "add"
_add = SProxy

type ADD r = (add :: FProxy AddF | r)

type Expr r = Run (ADD + r)

-- Smart Constructors

lit :: forall r a. a -> Run r a
lit n = pure n

add :: forall r a
     . Run (ADD + r) a
    -> Run (ADD + r) a
    -> Run (ADD + r) a
add x y = join $ lift _add (AddF x y)

iter :: forall r a. (VariantF r a -> a) -> Run r a -> a
iter k = go where
  go m = case peel m of
    Left f -> k (go <$> f)
    Right a -> a

expr :: forall r. Expr r Number
expr = add (lit 10.0) (lit 20.0)

eval :: forall r. Expr r Number -> Number
eval = iter (case_ # on _add evalAlgebra)
  where
    evalAlgebra (AddF a b) = a + b

main :: Effect Unit
main = do
  log "---- ADT9 ----"
  logShow $ eval expr
  log "--------------"
