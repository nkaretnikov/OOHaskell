{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Existential where

import Shape
import Circle
import Rectangle


-- An existential envelope for shapes

data AbstractShape = forall x. Shape x => AbstractShape x


-- Opaque shapes are shapes

instance Shape AbstractShape 
 where
  moveTo x y    (AbstractShape s) = AbstractShape $ moveTo x y s
  rMoveTo dx dy (AbstractShape s) = AbstractShape $ rMoveTo dx dy s
  draw          (AbstractShape x) = draw x


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
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = r { widthData = 30 } 
         draw r' 
