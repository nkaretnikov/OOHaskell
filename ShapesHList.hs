{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example.
We operate on a heterogeneous list of shapes.
In particular, we use a heterogeneous map over the list.
The polymorphic function of the map is a dedicated Apply instance.
We need to gather all used methods as constraints of the Apply instance.

-}

module ShapesHList where

import OOHaskell
import Shapes


-- The polymorphic scribble loop.

main =
  do
       -- set up list of shapes.
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = s1 `HCons` (s2 `HCons` HNil)
       
       -- iterate through the list
       -- and handle shapes polymorphically
       hMapM_ (undefined::ScribbleBody) scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
       arec # draw


-- A type code for the polymorphic function on shapes

data ScribbleBody -- a type code only!


-- The polymorphic function on shapes

instance ( HasField (Proxy Draw) r (IO ())
         , HasField (Proxy RMoveTo) r (Int -> Int -> IO ())
         )
      => Apply ScribbleBody r (IO ())
  where
    apply _ x = do
                   x # draw
                   (x # rMoveTo) 100 100
                   x # draw
