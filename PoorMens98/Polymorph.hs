module Polymorph where

import Shape
import Circle
import Rectangle


-- Tag the shape delta as needed for embedding

tagShape :: (w -> w') -> Shape w -> Shape w'
tagShape f s = s { rest = f (rest s) }

untagShape :: (Shape w -> t) -> (Shape w' -> t) -> Shape (Either w w') -> t
untagShape f g s
  = case rest s of
      (Left s')  -> f (s { rest = s' })
      (Right s') -> g (s { rest = s' })


-- Define draw for tagged shapes

instance (Draw a, Draw b) => Draw (Either a b)
 where
  draw = untagShape draw draw


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


-- ---------------------------------------------------------------------------
-- BiFunctors -- should be somewhere in library
-- not needed for this variation on the encoding

class BiFunctor f where
  bimap :: (a -> b) -> (c -> d) -> f a c -> f b d

instance BiFunctor Either where
  bimap f g (Left  x)  = Left (f x)
  bimap f g (Right x') = Right (g x')

bimap1 f g p1    = bimap (f p1)    (g p1)  
bimap2 f g p1 p2 = bimap (f p1 p2) (g p1 p2)  
