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


-- Weirich's / Rathman's test case

main =
  do
       s1 <- mfix $ rectangle 10 20 5 6
       s2 <- mfix $ circle 15 25 8
       let scribble = cons s1 (cons s2 nil)
       mapM_ (\x -> do
                       x # draw
                       (x # rMoveTo) 100 100
                       x # draw)
             scribble

      -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw
