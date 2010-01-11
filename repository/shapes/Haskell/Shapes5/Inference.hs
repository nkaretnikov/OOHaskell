
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Inference where

import Shape
import Circle
import Rectangle



-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ Left  (rectangle 10 20 5 6)
                        , Right (circle 15 25 8)
                        ]
         mapM_ (either scribbleBody scribbleBody) scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)
 where
   scribbleBody x =
     do
         draw x
         draw (rMoveTo 100 100 x) 
