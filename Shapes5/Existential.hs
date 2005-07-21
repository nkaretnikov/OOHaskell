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
  readShape  f (HideShape x) = readShape  f x
  writeShape f (HideShape x) = HideShape $ writeShape f x
  draw         (HideShape x) = draw x


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
         draw $ setWidth 30 (rectangle 0 0 15 15)
