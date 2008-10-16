{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
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

-}

module ShapesUnion where

import OOHaskell
import Shapes
import Data.HList.ConsUnion


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
       let scribble =  consEither s2
                      (consEither s1
                      (consEither s2
                      (consEither s3
                       nilEither)))

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


-- We instantiate the look-up (HasField) class for records.
-- We assure that the constructed unions form reasonable intersection type.

instance (HasField l x v, HasField l y v) 
       => HasField l (Either x y) v 
 where
  hLookupByLabel l (Left x)  =  hLookupByLabel l x
  hLookupByLabel l (Right y) =  hLookupByLabel l y

{-
We use downCast defined in HList's ConsUnion, which implicitly
uses TypeEq for the projection from the union. When dealing with objects
(records), we could replace that TypeEq with the subsumption
predicate that returns HTrue if the record contains all the fields of
the targeted type. Another design choice: we may assume the label name
to determine the type of the corresponding method. That forces the
width subtyping and consistent use of names, at least within one union
type. In that case, we can process even records with polymorphic
fields.
-}

