-- Haskell98!

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- Emulating Existential.hs without the use of existentials.
-- Closure is provides abstraction, too.

module Existential1 where

import Shape
import Circle
import Rectangle


-- An `existential' envelope for shapes

data AbstractShape = 
     AbstractShape { moveToA  :: Int -> Int -> AbstractShape
                   , rMoveToA :: Int -> Int -> AbstractShape
                   , drawA    :: IO ()}


-- Opaque shapes are shapes

instance Shape AbstractShape 
 where
  moveTo x y    s = moveToA s x y
  rMoveTo dx dy s = rMoveToA s dx dy
  draw          s = drawA s


-- The following two injections could be collected in a type class

abstractShape s =
  AbstractShape { moveToA  = \x y   -> abstractShape $ moveTo x y s
                , rMoveToA = \dx dy -> abstractShape $ rMoveTo dx dy s
                , drawA    = draw s }


-- Weirich's / Rathman's test case

main =
      do
         -- Handle the shapes polymorphically
         let scribble = [ abstractShape (rectangle 10 20 5 6)
                        , abstractShape (circle 15 25 8)
                        ]
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         let r = rectangle 0 0 15 15
         let r' = r { widthData = 30 } 
         draw r' 
