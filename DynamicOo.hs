{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2005, Oleg Kiselyov, Ralf Laemmel

-}


module DynamicOo where

import OOHaskell
import Record
-- import qualified Prelude (print)
-- import Prelude hiding (print)
import Data.Typeable
import Data.Dynamic


-- Up-cast

data UpCast x = UpCast x Dynamic -- Should be opaque!
upCast :: (Typeable (Record a), Narrow a b) => Record a -> UpCast (Record b)
upCast x = UpCast (narrow x) (toDyn x)


-- Down-cast

downCast :: (Typeable b, Narrow b a) => UpCast (Record a) -> Maybe (Record b)
downCast (UpCast _ d) = fromDynamic d


-- Method look-up for up-casted values

instance HasField l x v => HasField l (UpCast x) v
 where
  hLookupByLabel l (UpCast x _) =  hLookupByLabel l x


-- Some labels that derive Typeable

data Label1 deriving Typeable; label1 = proxy::Proxy Label1
data Label2 deriving Typeable; label2 = proxy::Proxy Label2
data Label3 deriving Typeable; label3 = proxy::Proxy Label3


-- A type hierarchy

type Super = Record ( Label1 :=: Bool :*: HNil )
type Sub1  = Record ( Label1 :=: Bool :*: Label2 :=: Int   :*: HNil )
type Sub2  = Record ( Label1 :=: Bool :*: Label3 :=: Float :*: HNil )


-- Some records which are "annotated" with their type

super = narrow (label1 .=. True .*. emptyRecord) :: Super
sub1  = narrow (label2 .=. 1 .*. super)          :: Sub1
sub2  = narrow (label3 .=. 2 .*. super)          :: Sub2


-- Some dynamics

dsuper = toDyn super
dsub1  = toDyn sub1
dsub2  = toDyn sub2


-- Some up-casted values

usub1 = upCast sub1 :: UpCast Super
usub2 = upCast sub2 :: UpCast Super


-- Demo

main = do 
           print $ dsuper
           print $ dsub1
           print $ dsub2
           print $ (fromDynamic dsub1 :: Maybe Sub1)
           print $ (fromDynamic dsub1 :: Maybe Sub2)
           print $ usub1 # label1
           print $ sub1  # label2
           print $ usub2 # label1
           print $ sub2  # label3
           print $ maybe Nothing
                         (\(x::Sub1) -> Just (x # label2))
                         (downCast usub1) 
           print $ maybe Nothing
                         (\(x::Sub1) -> Just (x # label2))
                         (downCast usub2) 
           print $ maybe Nothing
                         (\(x::Sub2) -> Just (x # label3))
                         (downCast usub1) 
           print $ maybe Nothing
                         (\(x::Sub2) -> Just (x # label3))
                         (downCast usub2) 
