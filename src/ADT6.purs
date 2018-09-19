module ADT6 where

import Prelude

import Data.Functor.Mu (Mu, roll)
import Data.Functor.Variant (FProxy, SProxy(..), VariantF, case_, expand, inj, match, on, onMatch)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Heterogeneous.Folding as H
import Matryoshka (Algebra, cata)
import Prim.Row as Row
import Type.Row (type (+))

-- Finally we should generalize your approach wich we described in ADT5.
-- In ADT5 we have some problem with recursive Coproduct
-- We shoud wrap/unwrap it everytime
-- Problem in this is that Coproduct is closed type sum
-- We can generalize idea with Coproduct to Variant
-- Variant is an opened type sum. Everytime we can add a new type to Variant
-- Variant and Record is opened type unlike Either and Tuple

-- Our general idea is extensible algebraic data types (EADT)

type EADT t = Mu (VariantF t)

injEADT :: forall f s a b. Row.Cons s (FProxy f) a b => IsSymbol s => Functor f => SProxy s -> Algebra f (EADT b)
injEADT label = roll <<< (inj label)

data LitF a = LitF Number
derive instance functorLitF :: Functor LitF

data AddF a = AddF a a
derive instance functorAddF :: Functor AddF

-- Rows
type Lit r = (lit :: FProxy LitF | r)
type Add r = (add :: FProxy AddF | r)

type ExprF r = Lit + Add + r
type Expr r = EADT (ExprF r)

-- Labels
_lit = SProxy :: SProxy "lit"
_add = SProxy :: SProxy "add"

-- Smart Constructors

lit :: forall r. Number -> EADT (Lit r)
lit n = injEADT _lit (LitF n)

add :: forall r. EADT (Add + r) -> EADT (Add + r) -> EADT (Add + r)
add x y = injEADT _add (AddF x y)

eval :: Expr () -> Number
eval = cata evalAlgebra where
  evalAlgebra :: Algebra (VariantF (ExprF + ())) Number
  evalAlgebra = match
    { lit: \(LitF x) -> x
    , add: \(AddF x y) -> x + y
    }

--

class Functor f <= EvalAlgebra f where
  evalAlgebra :: Algebra f Number

instance evalAlgebraLitF :: EvalAlgebra LitF where
  evalAlgebra (LitF x) = x

instance evalAlgebraAddF :: EvalAlgebra AddF where
  evalAlgebra (AddF x y) = x + y

eval' :: Expr () -> Number
eval' = cata evalAlgebra' where
  evalAlgebra' :: Algebra (VariantF (ExprF + ())) Number
  evalAlgebra' = match
    { lit: \x -> evalAlgebra x
    , add: \x -> evalAlgebra x
    }


data MulF a = MulF a a
derive instance functorMulF :: Functor MulF

type Mul r = (mul :: FProxy MulF | r)

-- Labels
_mul = SProxy :: SProxy "mul"

-- Smart Constructors

mul :: forall r. EADT (Mul + r) -> EADT (Mul + r) -> EADT (Mul + r)
mul x y = injEADT _mul (MulF x y)

type AdvExprF r = ExprF + Mul + r
type AdvExpr r = EADT (AdvExprF r)

alg :: forall r. Algebra (VariantF r) Number -> Algebra (VariantF (ExprF + r)) Number     
alg = onMatch
  { lit: case _ of LitF x -> x
  , add: case _ of AddF x y -> x + y }

alg2 :: forall r. Algebra (VariantF r) Number -> Algebra (VariantF (AdvExprF r)) Number 
alg2 = alg
  >>> on _mul case _ of MulF x y -> x * y

instance evalAlgebraMulF :: EvalAlgebra MulF where
  evalAlgebra (MulF x y) = x * y

-- 

data EvalCase = EvalCase

instance evalCase :: EvalAlgebra f => H.Folding EvalCase acc (f Number) Number where
  folding EvalCase _ = evalAlgebra

evalAlgebra'' :: forall r. H.HFoldl EvalCase Unit (VariantF (ExprF + r) Number) Number => Algebra (VariantF (ExprF + r)) Number
evalAlgebra'' = H.hfoldl EvalCase unit

-- eval'' :: forall r. Expr r -> Number
-- eval'' = cata evalAlgebra''

expr :: Expr ()
expr = add (lit 10.0) (lit 20.0)

expr2 :: AdvExpr ()
expr2 = add (lit 10.0) (lit 20.0)

main :: Effect Unit
main = do
  log "---- ADT6 ----"
  logShow $ eval expr
  logShow $ eval' expr
  logShow $ cata evalAlgebra'' expr
  logShow $ cata (case_ # alg) expr
  logShow $ cata ((case_ # alg2) <<< expand) expr
  logShow $ cata evalAlgebra'' expr2
  -- logShow $ eval'' expr
  log "--------------"