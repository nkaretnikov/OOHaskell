import Control.Monad.State
import Data.FiniteMap
import Data.Maybe

data Exp = Zero
         | Succ Exp
         | Then Exp Exp
         | Var Id
         | Assign Id Exp

type Id  = String
type Val = Int

interpret :: Exp -> State (FiniteMap Id Val) Val
interpret Zero         = return 0
interpret (Succ x)     = interpret x >>= return . (+) 1
interpret (Then x y)   = interpret x >>= const (interpret y)
interpret (Var i)      = get >>= return . fromJust . flip lookupFM i
interpret (Assign i x) =
  do 
     v <- interpret x
     m <- get
     let m' = addToFM m i v
     () <- put m'
     return v

main = print $ fst $ runState (interpret
         ( (Assign "a" (Succ Zero))
           `Then`
           (Assign "a" (Succ (Var "a")))
           `Then`
           Succ (Var "a") )
         ) emptyFM
