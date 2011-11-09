{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

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

module ShapesEither where

import OOHaskell
import ShapesBase
import Data.HList.ConsUnion


-- The explicit type of circles.
-- We can use this type in down-casting.
-- We can also get away w/o such an explicit type.
-- That is, we can do instance-based down-casting; see below.

type Circle a = Record (  GetRadius :=: IO a
                      :*: SetRadius :=: (a -> IO ())
                      :*: Draw      :=: IO ()
                      :*: GetX      :=: IO a
                      :*: GetY      :=: IO a
                      :*: SetX      :=: (a -> IO ())
                      :*: SetY      :=: (a -> IO ())
                      :*: MoveTo    :=: (a -> a -> IO ())
                      :*: MoveBy    :=: (a -> a -> IO ())
                      :*: HNil )


-- We need full instantiation for the downcasts to work.
-- ... or a better comparsion function. (Omitted.)

-- Test case for heterogeneous collections

main = do
          -- Construct a list of shapes
          s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
          s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
          s3 <- mfix (square (35::Int) (45::Int) (8::Int))
          s4 <- mfix (circle (42::Int) (88::Int) (77::Int))
          let scribble =  consEither s1
                         (consEither s2
                         (consEither s3
                         (consEither s4
                          nilEither)))

          -- Handle the shapes in the list polymorphically       
          mapM_ (\s -> do
                          s # draw
                          (s # moveBy) 100 100
                          s # draw)
                scribble

          -- Type-based bound for down-casting
          mapM_ (\s -> maybe (return ())
	                     (\(c::Circle Int) -> do
                                                     c # setRadius $ 10
			                             c # draw)
	                     (downCast s))
                scribble

          -- Value-based bound for down-casting
          mapM_ (\s -> maybe (return ())
	                     (\c -> do
                                       c # setRadius $ 10
			               c # draw)
	                     (downCast s `asTypeOf` Just s2))
                scribble

          -- call a rectangle specific function
          arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
          arec # setWidth $ 30
--          arec # setRadius $ 40
          arec # draw


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

