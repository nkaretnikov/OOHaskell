{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example.
We use an auto-LUBing narrow to prepare all objects before insertion into a list.
We first build a heterogeneous list of shapes.
Then we map over this list to narrow all shapes to the LUB type.
Thereby we obtain a normal homogeneous list.
BTW, hLubNarrow could just as well be a multi-variate function

-}

module ShapesLub where

import OOHaskell
import Shapes


-- The polymorphic scribble loop.

main =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = consLub s1 (consLub s2 nilLub)
       
       -- iterate through the array
       -- and handle shapes polymorphically
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
