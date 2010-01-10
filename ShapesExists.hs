{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

A variation on the shapes example.
We make shapes opaque in an existential envelope.
Thereby, all shapes can be inserted into a normal homogeneous list.

-}

module ShapesExists where

import OOHaskell
import ShapesBase hiding (main)


-- Test case for heterogeneous collections

main = do
          -- Construct a list of shapes
          s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
          s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
          let scribble = [ AnyShape s1
                         , AnyShape s2 ]
       
          -- Handle the shapes in the list polymorphically       
          mapM_ (\(AnyShape s) -> do
                                     s # draw
                                     (s # moveBy) 100 100
                                     s # draw )
                scribble

          -- call a rectangle specific function
          arec <- mfix (rectangle (0::Int) (0::Int) (15::Int) (15::Int))
          arec # setWidth $ 30
          arec # draw


-- An appropriately bounded existential wrapper for shapes

data AnyShape =
 forall x. ( HasField (Proxy Draw) x (IO ())
           , HasField (Proxy MoveBy) x (Int -> Int -> IO ())
           ) => AnyShape x
