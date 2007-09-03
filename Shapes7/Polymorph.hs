
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- BiFunctors

class BiFunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor Either where
  bimap f g (Left  x)  = Left (f x)
  bimap f g (Right x') = Right (g x')


-- Tagged shapes are shapes

instance (Shape a, Shape b) => Shape (Either a b)
 where
  moveTo x y = bimap (moveTo x y) (moveTo x y)
  rMoveTo dx dy = bimap (rMoveTo dx dy) (rMoveTo dx dy)
  draw = either draw draw


-- Up-cast operation

type AnyShape = Either RectangleData CircleData

class UpCastToShape x
 where
  upCastToShape :: x -> AnyShape

instance UpCastToShape RectangleData
 where
  upCastToShape = Left

instance UpCastToShape CircleData
 where
  upCastToShape = Right


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ upCastToShape  (rectangle 10 20 5 6)
                        , upCastToShape (circle 15 25 8) ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x) )
               scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = r { widthData = 30 } 
         draw r' 
