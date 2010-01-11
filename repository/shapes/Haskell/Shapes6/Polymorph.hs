{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Subtype
import Shape
import Circle
import Rectangle


-- We are going to build lists of drawables.

data Drawable = forall a. (Draw a, Start a)
  => Drawable a


-- The main function, very much like Rathman's.

main =
      do
         -- handle the shapes polymorphically
         drawloop scribble

         -- handle rectangle specific instance
         arectangle' <- setWidth 30 arectangle
         draw arectangle'

      where
         -- create some shape instances (using existential wrapper)
         scribble = [
            Drawable (Rectangle (Shape 10 20) 5 6),
            Drawable (Circle (Shape 15 25) 8)]

         -- create a rectangle instance
         arectangle = (Rectangle (Shape 0 0) 15 15)


-- Iterate through the list of shapes and draw
drawloop [] = return True
drawloop (Drawable x:xs) =
      do
         draw x
         x' <- rMoveTo 100 100 x
         draw x'
         drawloop xs
