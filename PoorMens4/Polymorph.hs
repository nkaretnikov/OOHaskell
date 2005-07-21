module Polymorph where

import Shape
import Circle
import Rectangle


-- Cast to a uniform base type
castToShape :: Shape w -> Shape ()
castToShape s = Shape { getX      = getX s
                      , getY      = getY s
                      , setX      = castToShape . setX s 
                      , setY      = castToShape . setY s 
                      , moveTo    = \z -> castToShape . moveTo s z 
                      , rMoveTo   = \z -> castToShape . rMoveTo s z 
                      , draw      = draw s
                      , shapeTail = ()
                      }


-- Weirich's / Rathman's test case

main =
      do

         -- Handle the shapes polymorphically
         let scribble = [ castToShape (rectangle 10 20 5 6)
                        , castToShape (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo x 100 100)
               )
               scribble

         -- Handle rectangle-specific instance
         draw (setWidth (rectangle 0 0 15 15) 30)
