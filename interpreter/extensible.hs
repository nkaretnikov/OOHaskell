{-
 
OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke
 
http://homepages.cwi.nl/~ralf/OOHaskell/

Incidentally, this example does *not* use OOHaskell. It rather deals
with some nice OOP programming bit that is readily enabled in Haskell
even without OOHaskell skills. We use Haskell's type classes to
program in a style of extensible datatypes. The example is about a
tiny interpreter, which we could extend by new syntax, whenever
necessary.

Also see file "nonextensible.hs".

-}


import Control.Monad.State
import Data.Map
import Data.Maybe

-- Class for expression syntax
class Exp x

-- Expression syntax so far; could be extended
data  Zero                       = Zero
data  Exp x          => Succ x   = Succ x
data  (Exp x, Exp y) => Then x y = Then x y
data                    Var      = Var Id
data  Exp x          => Assign x = Assign Id x

instance                   Exp Zero
instance Exp x          => Exp (Succ x)
instance (Exp x, Exp y) => Exp (Then x y)
instance                   Exp Var
instance Exp x          => Exp (Assign x)

type  Id  = String


-- Semantic domain
type  Val = Int


-- The monadic interpreter function

class Exp x => Interpret x
 where
  interpret :: x -> State (Map Id Val) Val

instance Interpret Zero
 where
  interpret Zero = return 0

instance Interpret x => Interpret (Succ x)
 where
  interpret (Succ x) = interpret x >>= return . (+) 1

instance (Interpret x, Interpret y) => Interpret (Then x y)
 where
  interpret (Then x y) = interpret x >>= const (interpret y)

instance Interpret Var
 where
  interpret (Var i) = get >>= return . fromJust . Data.Map.lookup i

instance Interpret x => Interpret (Assign x)
 where
  interpret (Assign i x) =
    do 
       v <- interpret x
       m <- get
       let m' = insert i v m
       () <- put m'
       return v


-- Test harness
main = print $ fst $ runState (interpret
         ( (Assign "a" (Succ Zero))
           `Then`
           (Assign "a" (Succ (Var "a")))
           `Then`
           Succ (Var "a") )
         ) Data.Map.empty
