{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example. We encode subtyping through
open-ended normal disjoint unions. When we attempt record look-up, we
simply require that both summands admit the look-up since we could
find that any summand is actually inhabited at the value level. Two
strengths of this approach are these: it readily allows for
down-casts, and and no nominal up-casts are required from the
programmer. Down-casts are completely type-safe; we can only attempt
to cast to types that are part of the union. In order to make all this
work, we need to define a few new type-level functions. These new
functions may eventually be moved to the HList or the OOHaskell
libraries.

The given encoding and its use in the shapes example is somewhat
inefficient in so far that the depth of the sum resembles the length
of the subtype-polymorphic list. It should be clear that the
construction of the sum type and its inhabitation (through the helper
eitherCons) was easily optimised to observe the existence of summands.

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
       let scribble =  eitherCons s2
                      (eitherCons s1
                      (eitherCons s2
                      [s3] ))

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


-- Normal list cons combined with embedding into a sum

eitherCons :: x -> [y] -> [Either x y]
eitherCons x ys = Left x : map Right ys


-- We instantiate the look-up (HasField) class for records.
-- We assure that the constructed unions form reasonable intersection type.

instance (HasField l x v, HasField l y v) 
       => HasField l (Either x y) v 
 where
  hLookupByLabel l (Left x)  =  hLookupByLabel l x
  hLookupByLabel l (Right y) =  hLookupByLabel l y


-- Down-cast a value of a union type to a summand type.
-- Make sure that the summand type occurs once at least.
-- Rely on a right associativity assumption for unions.

class DownCast u s
  where
    downCast :: u -> Maybe s

instance (DownCastEither b x y s, TypeEq x s b) 
      =>  DownCast (Either x y) s
  where
    downCast = downCastEither (undefined::b)

instance TypeCast x s
      => DownCast x s
  where
    downCast = Just . typeCast


-- Downcast a sum-typed value.
-- Make sure that the summand type occurs once at least.

class DownCastEither b x y s
  where
    downCastEither :: b -> Either x y -> Maybe s

instance DownCast y s
      => DownCastEither HFalse x y s
  where
    downCastEither _ (Left x)  = Nothing
    downCastEither _ (Right y) = downCast y

instance (TypeCast x s, DownCastTotal y s)
      =>  DownCastEither HTrue x y s
  where
    downCastEither _ (Left x)  = Just (typeCast x)
    downCastEither _ (Right y) = downCastTotal y


-- Down-cast a value of a union type to a summand type.
-- Do not insist on the summand type to be present.

class DownCastTotal u s
  where
    downCastTotal :: u -> Maybe s

instance (DownCastEitherTotal b x y s, TypeEq x s b) 
      =>  DownCastTotal (Either x y) s
  where
    downCastTotal = downCastEitherTotal (undefined::b)

instance (TypeCastBool b x s, TypeEq x s b)
      =>  DownCastTotal x s
  where
    downCastTotal = typeCastBool (undefined::b)


-- Type-level type cast; return Nothing if all fails.

class TypeCastBool b x y 
  where
    typeCastBool :: b -> x -> Maybe y

instance TypeCast x y
      => TypeCastBool HTrue x y
  where
    typeCastBool _ = Just . typeCast

instance TypeCastBool HFalse x y
  where
    typeCastBool _ = const Nothing


-- Downcast a sum-typed value.
-- Do not insist on the summand type to be present.

class DownCastEitherTotal b x y s
  where
    downCastEitherTotal :: b -> Either x y -> Maybe s

instance DownCastTotal y s
      => DownCastEitherTotal HFalse x y s
  where
    downCastEitherTotal _ (Left x)  = Nothing
    downCastEitherTotal _ (Right y) = downCastTotal y

instance (TypeCast x s, DownCastTotal y s)
      =>  DownCastEitherTotal HTrue x y s
  where
    downCastEitherTotal _ (Left x)  = Just (typeCast x)
    downCastEitherTotal _ (Right y) = downCastTotal y
