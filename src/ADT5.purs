module ADT5 where

import Prelude

import Data.Functor.Coproduct (Coproduct, coproduct, left, right)
import Data.Functor.Mu (Mu(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Matryoshka (class Recursive, cata)

-- Combine all your previous ideas

infixr 1 type Coproduct as +

data Lit a = Lit Number
data Add a = Add a a

instance functorLit :: Functor Lit where
  map f (Lit n) = Lit n

instance functorAdd :: Functor Add where
  map f (Add a b) = Add (f a) (f b)

class Functor f <= Eval f where
  evalAlgebra :: f Number -> Number

instance evalLit :: Eval Lit where
  evalAlgebra (Lit x) = x

instance evalAdd :: Eval Add where
  evalAlgebra (Add x y) = x + y

instance evalCoproduct :: (Eval f, Eval g) => Eval (f + g) where
  evalAlgebra = coproduct evalAlgebra evalAlgebra

eval :: forall t f. Recursive t f => Eval f => t -> Number
eval = cata evalAlgebra

expr :: Mu (Lit + Add)
expr = In (right (Add (In (left (Lit 10.0))) (In (left (Lit 20.0))))) -- This is pain!

-- Lets make our approach more humane

class (Functor sub, Functor sup) <= Inj sub sup where
  inj :: sub ~> sup
  prj :: forall a. sup a -> Maybe (sub a)

instance injId :: Functor f => Inj f f where
  inj = identity
  prj = Just

else instance injLeft :: (Functor f, Functor g) =>  Inj f (f + g) where
  inj = left
  prj = coproduct Just (const Nothing)

else instance injRight :: (Functor f, Functor g, Functor h, Inj f g) => Inj f (h + g) where
  inj = right <<< inj
  prj = coproduct (const Nothing) prj

inject :: forall g f. Inj g f => g (Mu f) -> Mu f
inject = In <<< inj

-- Smart constructors

lit :: forall f. Inj Lit f => Number -> Mu f
lit n = inject (Lit n)

add :: forall f. Inj Add f => Mu f -> Mu f -> Mu f
add a b = inject (Add a b)

expr2 :: Mu (Add + Lit)
expr2 = add (lit 10.0) (lit 20.0)

main :: Effect Unit
main = do
  log "---- ADT5 ----"
  logShow $ eval expr
  logShow $ eval expr2
  log "--------------"