
-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ Shape (rectangle 10 20 5 6)
                        , Shape (circle 15 25 8) ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x) )
               scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = set_width 30 r
         draw (Shape r')

