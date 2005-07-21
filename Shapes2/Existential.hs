{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle

-- Existential envelope for `drawables'

data Drawable = forall a. Draw a
  => Drawable (Shape a)


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ Drawable (rectangle 10 20 5 6)
                        , Drawable (circle 15 25 8)
                        ]
         mapM_ ( \(Drawable x) -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x))
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 arectangle

      where

         -- Create a rectangle instance
         arectangle = (rectangle 0 0 15 15)
