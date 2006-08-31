{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004-2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

A deep-subtyping variation on narrow from the HList library.

We do not place deep'narrow in HList/Record.hs because the kind of
type inspection (see IO) assumes mutable object types.  Hence, this
sort of narrowing is specific to the OOHaskell model. For the record,
we do not place deep'narrow in the OOHaskell main module because of
technicalities related to the implementation of TypeEq and TypeCast
(cf. the HList technical report).


-}

module DeepNarrow where

import CommonMain
import GhcSyntax

data ItsRecord
data ItsIO
data ItsFunction
data ItsOther

class TopTyCon a b | a -> b
instance TopTyCon (Record y) ItsRecord
instance TopTyCon (IO y) ItsIO
instance TopTyCon (a->b) ItsFunction
instance TypeCast f ItsOther => TopTyCon a f

class DeepNarrow a b where
    deep'narrow :: a -> b

instance (TopTyCon a f, DeepNarrow' f a b) => DeepNarrow a b where
    deep'narrow = deep'narrow' (undefined::f)

class DeepNarrow' f a b where
    deep'narrow' :: f -> a -> b

instance TypeCast a b => DeepNarrow' ItsOther a b where
    deep'narrow' _ = typeCast

-- Contra-variance on the argument type, co-variance in the result-type
-- The type-checker won't accept in any other way...

instance (DeepNarrow a' a, DeepNarrow b b')
    => DeepNarrow' ItsFunction (a->b) (a'->b') where
    deep'narrow' _ f = \x -> deep'narrow (f (deep'narrow x))


instance DeepNarrow' ItsRecord r (Record HNil) where
    deep'narrow' _ _ = emptyRecord

{-

Note: all of the following constraints were recovered by Haskell's
type inference. One starts by writing down the body of the function
deep'narrow' below, then the Haskell compiler is used to find missing
constraints which are then indeed added as to complete the instance.

-}

instance ( DeepNarrow' ItsRecord (Record r) (Record r')
         , H2ProjectByLabels (HCons l HNil) r (HCons (F l v) HNil) rout
	 , TopTyCon v f, DeepNarrow' f v v'
	 , HRLabelSet (HCons (F l v') r')
	 )
    => DeepNarrow' ItsRecord (Record r) (Record (HCons (F l v') r')) where
    deep'narrow' _ r = result
	where
	r'       = (deep'narrow r) :: (Record r')
	labels   = HCons (undefined::l) HNil
	Record (HCons (F v) HNil) = hProjectByLabels labels  r
	(v'::v') = deep'narrow v
	result   = (newF undefined v') .*. r'
		  
instance DeepNarrow a b => DeepNarrow' ItsIO (IO a) (IO b) where
    deep'narrow' _ a = a >>= (return . deep'narrow)

