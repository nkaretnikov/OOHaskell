{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import ShapesBase hiding (main)


-- Test case for heterogeneous collections

main = do
          -- Construct a list of shapes
          s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
          s2 <- mfix (circle (15::Int) 25 8)
          let scribble = s1 `HCons` (s2 `HCons` HNil)
       
          -- Handle the shapes in the list polymorphically       
          hMapM_ (undefined::ScribbleBody) scribble

          -- call a rectangle specific function
          arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
          arec # setWidth $ 30
          arec # draw


-- The argument of hMapM_

data ScribbleBody

instance ( HasField (Proxy Draw) r (IO ())
         , HasField (Proxy MoveBy) r (Int -> Int -> IO ())
         )
      => Apply ScribbleBody r (IO ())
  where
    apply _ s = do
                   s # draw
                   (s # moveBy) 100 100
                   s # draw
