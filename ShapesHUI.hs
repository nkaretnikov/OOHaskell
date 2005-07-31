{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example. We encoded subtyping through
particular intersection types.  That is, we use a TIC (i.e., a union
or variant type with unqiue summands) whose OO interface is the
intersection of the summand types. For short, we call these types
union-intersection types: UI types. The strong aspect of this encoding
is that down-casts remain possible *and* no nominal casts are required
from the programmer. Downcasts are completely type-safe; we can only
attempt to cast to types that are part of the union. In order to make
all this work, we need to define several new type-level functions. The
idea is that these new functions will eventually be moved to the the
HList or the OOHaskell libraries.

-}

module ShapesIntersect where

import OOHaskell
import Shapes


-- The polymorphic scribble loop.

main =
  do

-- Encoding 5'
-- We build a homogeneous list of shapes.
-- This is similar to encoding 5, only with a different universe
-- and different injection/projection
-- (HCons a (HCons b HNil)) --> [(HCons (Maybe a) (HCons (Maybe b) HNil))]
-- Encoding 5 is more efficient, yet the iso-morphism in encoding 5'
-- seems a bit more elegant

stynamicHDowncast =
  do
       -- set up array of shapes
       -- Need full instantiation for the downcasts to work...
       -- Actually, I just need a better comparsion function, but I'm
       -- in a hurry
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       s3 <- mfix (square (35::Int) (45::Int) (8::Int))
       -- could have used vararg function
       let scribble = hunion'inter (HCons s2 
				    (HCons s1 (HCons s2 (HCons s3 HNil))))
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       putStrLn "DownCast to Circle"
       -- iterate through the array and downcast
       mapM_ (\shape -> maybe (putStrLn "Not a circle")
	                      (\circ -> do circ # setRadius $ 10;
			                   circ # draw)
	                      ((hdowncast shape) `asTypeOf` (Just s2)))
             scribble

       putStrLn "DownCast to Square"
       -- iterate through the array and downcast
       mapM_ (\shape -> maybe (putStrLn "Not a square")
	                      (\sq -> do sq # setWidth $ 10;
			                 sq # draw)
	                      ((hdowncast shape) `asTypeOf` (Just s3)))
             scribble


newtype HUnionIntersection u = HUnionIntersection u
unHUI (HUnionIntersection x) = x

class HUI l r | l -> r where
    hunion'inter :: l -> [HUnionIntersection r]

instance HUI (HCons obj HNil) (HCons (Maybe obj) HNil) where 
    hunion'inter (HCons obj HNil) = 
	[HUnionIntersection (HCons (Just obj) HNil)]

instance HUI (HCons o2 t) r => 
    HUI (HCons o1 (HCons o2 t)) (HCons (Maybe o1) r) where
    hunion'inter (HCons o t) = 
	let ut = hunion'inter t
	in HUnionIntersection (HCons (Just o) undefined) :
	   map (HUnionIntersection . (HCons Nothing) . unHUI) ut

-- This essentially computes the intersection. Only width sub-typing
-- at present

instance HasField l a v => 
    HasField l (HUnionIntersection (HCons (Maybe a) HNil)) v 
 where
  hLookupByLabel l (HUnionIntersection (HCons (Just a) _)) 
      =  hLookupByLabel l a

instance (HasField l a v, 
	  HasField l (HUnionIntersection (HCons b r)) v)
    => HasField l (HUnionIntersection (HCons (Maybe a) (HCons b r))) v 
 where
  hLookupByLabel l (HUnionIntersection (HCons a r)) 
      =  maybe (hLookupByLabel l (HUnionIntersection r))
	       (hLookupByLabel l) a

hdowncast f = hdowncast1 (undefined::HFalse) f

-- The seen flag (which is an HBool) is for a reason that a type
-- may not be a unique index in the HList (the list may have duplicates)
-- So, we keep track if we have already seen the type
class HDownCast seen f t where
    hdowncast1 :: seen -> HUnionIntersection f -> Maybe t

-- Here, TypeEq can be replaced with the subsumption predicate
-- that returns HBool bf if the record a contains all the fields of t
-- Another design choice: we may assume that the label name determines
-- the type of the corresponding method. That forces the width subtyping
-- and consistent use of names, at least within one HUnionIntersection
-- hierarchy. In that case, we can process even records with polymorphic
-- fields.
instance (TypeEq a t bf, HDowncast' bf seen (HCons (Maybe a) r) t)
    => HDownCast seen (HCons (Maybe a) r) t where
    hdowncast1 = hdowncast1' (undefined::bf)

class HDowncast' bf seen f t where
    hdowncast1' :: bf -> seen -> HUnionIntersection f -> Maybe t

instance HDowncast' HTrue seen (HCons (Maybe a) HNil) a where
    hdowncast1' _ _ (HUnionIntersection (HCons ma _)) = ma

instance HDownCast HTrue (HCons b r) a
    => HDowncast' HTrue seen (HCons (Maybe a) (HCons b r)) a where
    hdowncast1' _ _ (HUnionIntersection (HCons ma r)) = 
	maybe (hdowncast1 (undefined::HTrue) (HUnionIntersection r)) Just ma 

instance HDownCast seen (HCons b r) t
    => HDowncast' HFalse seen (HCons (Maybe a) (HCons b r)) t
    where
    hdowncast1' _ seen (HUnionIntersection (HCons ma r)) = 
	maybe (hdowncast1 seen (HUnionIntersection r)) (const Nothing) ma

instance HDowncast' HFalse HTrue (HCons a HNil) t
    where
    hdowncast1' _ _ _ = Nothing
{-
-- We can comment out the latter instance. In that case, the downcast
-- is truly stynamic: it will statically fail if it is clear statically
-- that the downcast will fail
instance HDowncast' HFalse HFalse (HCons a HNil) t
    where
    hdowncast1' _ _ _ = Nothing
-}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


{--
main = do 
          putStrLn "testCoerce"; myShapesOOP
          putStrLn "testLub"; yaShapesOOP
          putStrLn "testHList";  testHList
          putStrLn "testExist";  testExist
          putStrLn "stynamicDowncast";  testExist

-- :t mfix $ rectangle (1::Int) (2::Int) (3::Int) (4::Int)

--}
