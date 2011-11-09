{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

{-

-- (C) 2004-2010, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example.
We use narrow (coerce) to prepare all objects before insertion into a list.

-}

module ShapesNarrow (main) where

import OOHaskell
import ShapesBase


-- When we want to make a list of Shapes, then we cast objects to this interface.

-- The interface of all shapes

type Shape a = Record (  GetX    :=: IO a
                     :*: GetY    :=: IO a
                     :*: SetX    :=: (a -> IO ())
                     :*: SetY    :=: (a -> IO ())
                     :*: MoveTo  :=: (a -> a -> IO ())
                     :*: MoveBy  :=: (a -> a -> IO ())
                     :*: Draw    :=: IO ()
                     :*: HNil )


-- In fact this interface would be sufficent.

type Shape' a
 = Record (  MoveBy  :=: (a -> a -> IO ())
         :*: Draw    :=: IO ()
         :*: HNil )


-- Test case for heterogeneous collections

main = do
          -- Construct a list of shapes
          s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
          s2 <- mfix (circle (15::Int) 25 8)
          let scribble :: [Shape Int]
              scribble = [narrow s1, narrow s2]

          -- Handle the shapes in the list polymorphically       
          mapM_ (\s -> do
                          s # draw
                          (s # moveBy) 100 100
                          s # draw)
                scribble

          -- call a rectangle specific function
          arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
          arec # setWidth $ 30
--           arec # setRadius $ 40
          arec # draw
