
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- Define a closed union over kinds of shapes

type AllShapes w = Shape (Either (RectangleDelta w) (CircleDelta w))


-- Define overloaded embedding into union

class UpCastToShape w
 where
  upCastToShape :: Shape (w w') -> AllShapes w'

instance UpCastToShape RectangleDelta
 where
  upCastToShape = tagShape Left

instance UpCastToShape CircleDelta
 where
  upCastToShape = tagShape Right


-- Tag the shape delta as needed for embedding into Either

tagShape :: (w -> w') -> Shape w -> Shape w'
tagShape f s = s { setX      = tagShape f . setX s
                 , setY      = tagShape f . setY s
                 , moveTo    = \z -> tagShape f . moveTo s z
                 , rMoveTo   = \z -> tagShape f . rMoveTo s z
                 , shapeTail = f (shapeTail s) }


-- Narrow shapes to a uniform base type

narrowToShape :: Shape w -> Shape ()
narrowToShape s = s { setX      = narrowToShape . setX s
                    , setY      = narrowToShape . setY s 
                    , moveTo    = \z -> narrowToShape . moveTo s z 
                    , rMoveTo   = \z -> narrowToShape . rMoveTo s z
                    , shapeTail = () }


-- Weirich's / Rathman's test case

main' =
      do

         -- Handle the shapes polymorphically
         let scribble = [ upCastToShape (rectangle 10 20 5 6)
                        , upCastToShape (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo x 100 100)
               )
               scribble

         -- Handle rectangle-specific instance
         draw (setWidth (rectangle 0 0 15 15) 30)

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
