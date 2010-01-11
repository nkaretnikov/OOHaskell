
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- Define a closed union over kinds of shapes

type AnyShape w = Shape (Either (RectangleDelta w) (CircleDelta w))


-- Define overloaded embedding into union

class UpCastToShape w where
  upCastToShape :: Shape (w w') -> AnyShape w'

instance UpCastToShape RectangleDelta where
  upCastToShape = tagShape Left

instance UpCastToShape CircleDelta where
  upCastToShape = tagShape Right


-- Tag the shape delta as needed for embedding into Either

tagShape :: (w -> w') -> Shape w -> Shape w'
tagShape f s = s { shapeTail = f (shapeTail s) }


-- Define draw for tagged shapes

instance (Draw a, Draw b) => Draw (Either a b) where
  draw = eitherShape draw draw


-- Discriminate on Either-typed tail of shape

eitherShape :: (Shape w -> t) -> (Shape w' -> t) -> Shape (Either w w') -> t
eitherShape f g s
  = case shapeTail s of
      (Left s')  -> f (s { shapeTail = s' })
      (Right s') -> g (s { shapeTail = s' })


-- Weirich's / Rathman's test case

main = do
         -- Handle the shapes polymorphically
         let scribble = [ upCastToShape (rectangle 10 20 5 6)
                        , upCastToShape (circle 15 25 8)
                        ]
         mapM_ (\x ->  do draw x
                          draw (rMoveTo 100 100 x))
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)
