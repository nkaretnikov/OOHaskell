{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example. We encode subtyping through
particular intersection types.  That is, we (encode as if we) use a
TIC (i.e., a union or variant type with unqiue summands) whose OO
interface is the intersection of the summand types. We use the term
"union intersection types" (UI types) to denote these types. The
strong aspect of this encoding is that down-casts remain possible and
no nominal up-casts are required from the programmer. Down-casts are
completely type-safe; we can only attempt to cast to types that are
part of the union. In order to make all this work, we need to define
several new type-level functions. These new functions may be
eventually moved to the HList or the OOHaskell libraries.

-}

module ShapesIntersect where

import OOHaskell
import Shapes
import TypeCastGeneric2


-- The polymorphic scribble loop.

main =
  do
       --
       -- Set up array of shapes.
       -- We need full instantiation for the downcasts to work.
       -- ... or a better comparsion function. (Omitted.)
       --
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       s3 <- mfix (square (35::Int) (45::Int) (8::Int))

       -- We could have used a vararg function.
       let scribble = union'inter (HCons s2 
			          (HCons s1
                                  (HCons s2
                                  (HCons s3
                                   HNil))))

       -- Iterate through the array
       -- and handle shapes polymorphically.
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw

       -- iterate through the array and downcast to cirlce
       mapM_ (\shape -> maybe (putStrLn "Not a circle.")
	                      (\circ -> do circ # setRadius $ 10;
			                   circ # draw)
	                      ((downCast shape) `asTypeOf` (Just s2)))
             scribble


-- Construct normal list from HList

class UI l r | l -> r
  where
    union'inter :: l -> [r]


--
-- The following encoding does not care about type doubles.
-- Hence, the depth of the sum equals the number of list elements.
-- This could be clearly optimised, if necessary.
--

instance UI (HCons e HNil) e
  where 
    union'inter (HCons e HNil) = [e]

instance UI (HCons e2 t) r => 
         UI (HCons e1 (HCons e2 t)) (Either e1 r)
  where
    union'inter (HCons h t) = let t' = union'inter t
			      in (Left h) : (map Right t')


--
-- We also instantiate the look-up (HasField) class for records.
-- Here we essentially compute the intersection.
-- This is literally expressed in the instance for Either.
--

instance (HasField l a v, HasField l b v) 
       => HasField l (Either a b) v 
 where
  hLookupByLabel l (Left a)  =  hLookupByLabel l a
  hLookupByLabel l (Right b) =  hLookupByLabel l b


-- Down-cast a value of an UI type to a summand type

class DownCast f t
  where
    downCast :: f -> Maybe t

instance (DownCastEither bf a b t, TypeEq a t bf) 
      =>  DownCast (Either a b) t
  where
    downCast = downCastEither (undefined::bf)

instance (DownCastOne bf a t, TypeEq a t bf)
      =>  DownCast a t
  where
    downCast = downCastOne (undefined::bf)


-- Downcast a type, as is.

class DownCastOne bf a t
  where
    downCastOne :: bf -> a -> Maybe t

instance TypeCast a t
      => DownCastOne HTrue a t
  where
    downCastOne _ = Just . typeCast

instance DownCastOne HFalse a t
  where
    downCastOne _ = const Nothing


-- Downcast a sum-typed value

class DownCastEither bf a b t
  where
    downCastEither :: bf -> Either a b -> Maybe t

instance (DownCast b t, TypeCast a t)
      =>  DownCastEither HTrue a b t
  where
    downCastEither _ (Left x)   = Just (typeCast x)
    downCastEither _ (Right x') = downCast x'

instance DownCast b t
      => DownCastEither HFalse a b t
  where
    downCastEither _ (Left x)   = Nothing
    downCastEither _ (Right x') = downCast x'
