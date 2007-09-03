{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Existential where

import Shape
import Circle
import Rectangle


-- An existential envelope for shapes

data OpaqueShape = forall x. Shape x => HideShape x


-- Opaque shapes are shapes

instance Shape OpaqueShape 
 where
  moveTo x y    (HideShape s) = HideShape $ moveTo x y s
  rMoveTo dx dy (HideShape s) = HideShape $ rMoveTo dx dy s
  draw          (HideShape x) = draw x


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ HideShape (rectangle 10 20 5 6)
                        , HideShape (circle 15 25 8)
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
