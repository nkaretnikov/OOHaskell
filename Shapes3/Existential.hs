{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Existential where

import Shape hiding (draw,moveTo,rMoveTo)
import qualified Shape (draw,moveTo,rMoveTo)
import Circle
import Rectangle

data AbstractShape = forall x. AbstractShape (Shape x)

draw (AbstractShape s) = Shape.draw s
moveTo (AbstractShape s) x y = AbstractShape $ Shape.moveTo s x y
rMoveTo (AbstractShape s) dx dy = AbstractShape $ Shape.rMoveTo s dx dy


-- Weirich's / Rathman's test case

main =
      do

         -- Handle the shapes polymorphically
         let scribble = [ AbstractShape (rectangle 10 20 5 6)
                        , AbstractShape (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo x 100 100)
               )
               scribble

         -- Handle rectangle-specific instance
         Shape.draw (setWidth (rectangle 0 0 15 15) 30)

