{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

module Term where

class Term c a | c->a

newtype Lit a = Lit a
newtype Inc a = Inc a
newtype IsZ a = IsZ a
newtype If  a b c = If (a,b,c)
newtype Fst a = Fst a
newtype Snd a = Snd a
newtype Pair a b = Pair (a,b)

instance Term (Lit Int) Int
instance Term a Int => Term (Inc a) Int
instance Term a Int => Term (IsZ a) Bool
instance (Term t Bool, Term b1 a, Term b2 a) => Term (If t b1 b2) a
instance (Term t1 a, Term t2 b) => Term (Pair t1 t2) (a,b)
instance Term t (a,b) => Term (Fst t) a
instance Term t (a,b) => Term (Snd t) b

class Term t a => Eval t a | t->a where
     eval:: t -> a

instance Term (Lit a) a => Eval (Lit a) a where eval (Lit i) = i
instance Eval t Int => Eval (Inc t) Int where eval (Inc t) = eval t + 1
instance Eval t Int => Eval (IsZ t) Bool where eval (IsZ t) = eval t == 0
instance (Eval t Bool, Eval b1 a, Eval b2 a) => Eval (If t b1 b2) a where
    eval (If (t,b1,b2)) = if eval t then eval b1 else eval b2
instance (Eval t1 a, Eval t2 b) => Eval (Pair t1 t2) (a,b) where
    eval (Pair (t1,t2)) = (eval t1, eval t2)
instance Eval t (a,b) => Eval (Fst t) a where eval (Fst t) = fst (eval t)
instance Eval t (a,b) => Eval (Snd t) b where eval (Snd t) = snd (eval t)

t1 = If ( (IsZ (Fst (Pair (Lit (0::Int), Lit (1::Int)))))
        , (Inc (Fst (Pair (Lit (0::Int), Lit (1::Int)))))
        , (Inc (Snd (Pair (Lit (0::Int), Lit (1::Int))))) )


et1 = eval t1
