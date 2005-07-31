{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example.
We use narrow (coerce) to prepare all objects before insertion into a list.

-}

module ShapesNarrow where

import OOHaskell
import Shapes


-- When we want to make a list of Shapes, then we cast objects to this interface.

type Shape a = Record (  GetX    :=: IO a
                     :*: GetY    :=: IO a
                     :*: SetX    :=: (a -> IO ())
                     :*: SetY    :=: (a -> IO ())
                     :*: MoveTo  :=: (a -> a -> IO ())
                     :*: RMoveTo :=: (a -> a -> IO ())
                     :*: Draw    :=: IO ()
                     :*: HNil )


-- In fact this interface would be sufficent.

type Shape' a
 = Record (  (Proxy RMoveTo , a -> a -> IO ())
         :*: (Proxy Draw    , IO ())
         :*: HNil )


-- The polymorphic scribble loop.

main =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble :: [Shape Int]
           scribble = [narrow s1, narrow s2]
       
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
