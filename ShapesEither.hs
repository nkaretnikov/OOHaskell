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
down-casts, and no nominal up-casts are required from the
programmer. Down-casts are completely type-safe; we can only attempt
to cast to types that are part of the union. In order to make all this
work, we need to define a few new type-level functions. These new
functions may eventually be moved to the HList or the OOHaskell
libraries.

The given encoding and its use in the shapes example is somewhat
inefficient in so far that the depth of the sum resembles the length
of the subtype-polymorphic list. It should be clear that the
construction of the sum type and its inhabitation (through the helper
eitherCons) were easily optimised to observe the existence of
summands. We leave this as an exercise to the reader.

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
-- (To this end we use a type-level Boolean "type seen".)
-- We rely on right associativity for unions.

downCast = downCastSeen hFalse

class DownCastSeen seen u s
  where
    downCastSeen :: seen -> u -> Maybe s

{-

In the following, we could replace TypeEq with the subsumption
predicate that returns HTrue if the record contains all the fields of
the targeted type. Another design choice: we may assume the label name
to determine the type of the corresponding method. That forces the
width subtyping and consistent use of names, at least within one union
type. In that case, we can process even records with polymorphic
fields.

-}

instance (DownCastEither seen b x y s, TypeEq x s b) 
      =>  DownCastSeen seen (Either x y) s
  where
    downCastSeen seen = downCastEither seen (undefined::b)

instance (TypeCastSeen seen b x s, TypeEq x s b)
      =>  DownCastSeen seen x s
  where
    downCastSeen seen = typeCastSeen seen (undefined::b)


-- Type-level type cast.
-- Insist on total cast if we haven't seen the type in question.
-- Return potentially Nothing otherwise,

class TypeCastSeen seen b x y 
  where
    typeCastSeen :: seen -> b -> x -> Maybe y

instance TypeCast x y
      => TypeCastSeen seen HTrue x y
  where
    typeCastSeen _ _ = Just . typeCast

instance TypeCastSeen HTrue HFalse x y
  where
    typeCastSeen _ _ = const Nothing


-- Downcast a sum-typed value.

class DownCastEither seen b x y s
  where
    downCastEither :: seen -> b -> Either x y -> Maybe s

instance (DownCastSeen HTrue y s, TypeCast x s)
      =>  DownCastEither seen HTrue x y s
  where
    downCastEither _ _ (Left x)  = Just (typeCast x)
    downCastEither _ _ (Right y) = downCastSeen hTrue y

instance DownCastSeen seen y s
      => DownCastEither seen HFalse x y s
  where
    downCastEither _ _    (Left x)  = Nothing
    downCastEither seen _ (Right y) = downCastSeen seen y
