
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- Narrow shapes to a uniform base type

narrowToShape :: Shape w -> Shape ()
narrowToShape s = s { setX      = narrowToShape . setX s
                    , setY      = narrowToShape . setY s 
                    , moveTo    = \z -> narrowToShape . moveTo s z 
                    , rMoveTo   = \z -> narrowToShape . rMoveTo s z
                    , shapeTail = ()
                    }


-- Weirich's / Rathman's test case

main =
      do

         -- Handle the shapes polymorphically
         let scribble = [ narrowToShape (rectangle 10 20 5 6)
                        , narrowToShape (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo x 100 100)
               )
               scribble

         -- Handle rectangle-specific instance
         draw (setWidth (rectangle 0 0 15 15) 30)
