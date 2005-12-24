{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2005, Oleg Kiselyov, Ralf Laemmel

-}


module DynamicOo where

import OOHaskell
import Record


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

usub1 = dynUpCast sub1 :: DynUpCast Super
usub2 = dynUpCast sub2 :: DynUpCast Super


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
                         (dynDownCast usub1) 
           print $ maybe Nothing
                         (\(x::Sub1) -> Just (x # label2))
                         (dynDownCast usub2) 
           print $ maybe Nothing
                         (\(x::Sub2) -> Just (x # label3))
                         (dynDownCast usub1) 
           print $ maybe Nothing
                         (\(x::Sub2) -> Just (x # label3))
                         (dynDownCast usub2) 
