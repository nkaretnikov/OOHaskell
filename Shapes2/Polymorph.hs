
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Polymorph where

import Shape
import Circle
import Rectangle


-- Tag the shape delta as needed for embedding into Either

tagShape :: (w -> w') -> Shape w -> Shape w'
tagShape f s = s { shapeTail = f (shapeTail s) }


-- Discriminate on Either-typed tail of shape

eitherShape :: (Shape w -> t) -> (Shape w' -> t) -> Shape (Either w w') -> t
eitherShape f g s
  = case shapeTail s of
      (Left s')  -> f (s { shapeTail = s' })
      (Right s') -> g (s { shapeTail = s' })


-- Define draw for tagged shapes

instance (Draw a, Draw b) => Draw (Either a b)
 where
  draw = eitherShape draw draw


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ tagShape Left  (rectangle 10 20 5 6)
                        , tagShape Right (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)
