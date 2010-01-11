{-
 
OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke
 
http://homepages.cwi.nl/~ralf/OOHaskell/

Incidentally, this example does *not* use OOHaskell. It rather deals
with some nice OOP programming bit that is readily enabled in Haskell
even without OOHaskell skills. Incidentally, this file does *not* even
show that nice OOP bit, but it rather shows how we normally program in
Haskell, and we would appreciate extensibility. That is, this file
shows a tiny interpreter, which is extensible in the dimension of the
semantic domain (because we use monads), but nonextensible in the
dimension of syntax (because Haskell's datatypes are not exactly
extensible as of today).

Also see file "extensible.hs".

-}


import Control.Monad.State
import Data.Map
import Data.Maybe


-- Expression syntax
data Exp = Zero
         | Succ Exp
         | Then Exp Exp
         | Var Id
         | Assign Id Exp

type Id  = String


-- Semantic domain
type Val = Int


-- The monadic interpreter function
interpret :: Exp -> State (Map Id Val) Val
interpret Zero         = return 0
interpret (Succ x)     = interpret x >>= return . (+) 1
interpret (Then x y)   = interpret x >>= const (interpret y)
interpret (Var i)      = get >>= return . fromJust . Data.Map.lookup i
interpret (Assign i x) =
  do 
     v <- interpret x
     m <- get
     let m' = Data.Map.insert i v m
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
