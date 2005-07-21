
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
  readShape  f = either (readShape f)  (readShape f)
  writeShape f = bimap  (writeShape f) (writeShape f)
  draw         = either draw draw


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ Left  (rectangle 10 20 5 6)
                        , Right (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)
