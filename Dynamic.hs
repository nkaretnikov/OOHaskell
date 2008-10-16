{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

Support up-/down-casts based on dynamics.

-}


module Dynamic (
 module Dynamic
) where


import Data.Typeable
import Data.Dynamic
import Data.HList.Record
import Data.HList.GhcRecord


-- Up-cast

data DynUpCast x = DynUpCast x Dynamic deriving Typeable -- Should be opaque!
dynUpCast :: (Typeable (Record a), Narrow a b) => Record a -> DynUpCast (Record b)
dynUpCast x = DynUpCast (narrow x) (toDyn x)


-- Down-cast

dynDownCast :: (Typeable b, Narrow b a) => DynUpCast (Record a) -> Maybe (Record b)
dynDownCast (DynUpCast _ d) = fromDynamic d


-- Method look-up for up-casted values

instance HasField l x v => HasField l (DynUpCast x) v
 where
  hLookupByLabel l (DynUpCast x _) =  hLookupByLabel l x
