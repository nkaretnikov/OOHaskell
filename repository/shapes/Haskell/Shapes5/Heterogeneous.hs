{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Heterogeneous where

import MainGhcGeneric1
import Shape
import Circle
import Rectangle


-- A type code for the body of the loop

data ScribbleBody


-- The actual loop body

instance Shape s => Apply ScribbleBody s (IO ())
 where
  apply _ x = 
     do
        draw x
        draw (rMoveTo 100 100 x)


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble =  HCons (rectangle 10 20 5 6)
                        (HCons (circle 15 25 8)
                         HNil)
         hMapM_ (undefined::ScribbleBody) scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)
