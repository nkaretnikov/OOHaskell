{-# OPTIONS -fglasgow-exts #-}

module Polymorph where

import Shape
import Circle
import Rectangle

{-

   Compared to the "type extension by polymorphic record tails"
   approach, the only difference is that we do not necessarily face
   Shape records but potentially also records that contain Shape
   records. The only place where this detail matters is in the
   definition of the existential envelope.

-}

-- Existential envelope for `drawables'
data Drawable = forall a. Draw a
  => Drawable a

-- Weirich's / Rathman's test case
main =
      do
         -- Handle the shapes polymorphically
         mapM_ ( \(Drawable x) -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x))
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 arectangle

      where
         -- Create some shape instances
         scribble = [
            Drawable (rectangle 10 20 5 6),
            Drawable (circle 15 25 8)]

         -- Create a rectangle instance
         arectangle = (rectangle 0 0 15 15)
