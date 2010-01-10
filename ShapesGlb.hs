{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example. Bringing two records to the same
record type by automatically computing their record union (or the greatest
lower bound). This is the dual to consLub, which computes the
lowest upper bound of two record types.

Cf. consEither, which too computes the union of types -- of any types,
not necessarily records type. The present consGlb deals only with record
types ans computes the union of fields.

Like consEither (but _unlike_ consLub), we can project from the union to the
individual components. We can have a safe downcast.

Unlike consEither, we can project to the record type that did _not_
exist at the time web built the union.
Need a good example for that! Shapes aren't very useful for that example.

When we attempt record look-up, we
simply require that both summands admit the look-up since we could
find that any summand is actually inhabited at the value level. Two
strengths of this approach are these: it readily allows for
down-casts, and no nominal up-casts are required from the
programmer. Down-casts are completely type-safe; we can only attempt
to cast to types that are part of the union.

-}

module ShapesGlb where

import OOHaskell
import ShapesBase hiding (main)


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
       let scribble =  consGlb s2
                      (consGlb s1
                      (consGlb s2
                      (consGlb s3
                       nil)))

       -- Iterate through the array
       -- and handle shapes polymorphically.
       mapM_ (\shape -> do
                           shape # draw
                           (shape # moveBy) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw

       -- iterate through the array and `downcast' to circle
       -- Each member is a circle!
       mapM_ (\shape -> do shape # setRadius $ 10;
	                   shape # draw)
             scribble



-- We don't need NilGlb as it is the same as NilLub
-- data NilGlb 

class ConsGLB r rs rs' | r rs -> rs' where
    consGlb :: r -> rs -> rs'

instance ConsGLB r NilLub [r] where
    consGlb r _ = [r]

instance UnionSymRec r ru ru' => ConsGLB r [ru] [ru'] where
    consGlb r (ru:rus) = ru1 : ru2 : map inr rus
      where (ru1,ru2) = unionSR r ru
	    inr r2 = snd $ unionSR r r2



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

