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
data ItsOther

class IsIORecord a b | a -> b
instance IsIORecord (Record y) ItsRecord
instance IsIORecord (IO y) ItsIO
instance TypeCast f ItsOther => IsIORecord a f

class DeepNarrow a b where
    deep'narrow :: a -> b

instance (IsIORecord a f, DeepNarrow' f a b) => DeepNarrow a b where
    deep'narrow = deep'narrow' (undefined::f)

class DeepNarrow' f a b where
    deep'narrow' :: f -> a -> b

instance TypeCast a b => DeepNarrow' ItsOther a b where
    deep'narrow' _ = typeCast

instance DeepNarrow' ItsRecord r (Record HNil) where
    deep'narrow' _ _ = emptyRecord

{-

Note: all of the following constraints were recovered by Haskell's
type inference. One starts by writing down the body of the function
deep'narrow' below, then the Haskell compiler is used to find missing
constraints which are then indeed added as to complete the instance.

-}

instance ( DeepNarrow' ItsRecord (Record r) (Record r')
         , H2ProjectByLabels (HCons l HNil) r (HCons (l, v) HNil) rout
	 , IsIORecord v f, DeepNarrow' f v v'
	 , HRLabelSet (HCons (l,v') r')
	 )
    => DeepNarrow' ItsRecord (Record r) (Record (HCons (l,v') r')) where
    deep'narrow' _ r = result
	where
	r'       = (deep'narrow r) :: (Record r')
	labels   = HCons (undefined::l) HNil
	Record (HCons (l,v) HNil) = hProjectByLabels labels  r
	(v'::v') = deep'narrow v
	result   = (l,v') .*. r'
		  
instance DeepNarrow a b => DeepNarrow' ItsIO (IO a) (IO b) where
    deep'narrow' _ a = a >>= (return . deep'narrow)
