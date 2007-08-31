
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module ConsEither where

import Shape
import Circle
import Rectangle
import Polymorph hiding (main)


-- We can actually abstract from the number of summands

main =
      do
           -- Handle the shapes polymorphically
           let scribble = consEither
                           (rectangle 10 20 5 6)
                           [circle 15 25 8]
           mapM_ ( \x -> 
                    do
                       draw x
                       draw (rMoveTo 100 100 x) )
                 scribble

           -- Handle rectangle-specific instance
           draw $ setWidth 30 (rectangle 0 0 15 15)

-- A union-constructing cons operation
consEither :: h -> [t] -> [Either h t]
consEither h t@(_:_) = Left h : map Right t 
consEither _ _ = error "Cannot cons with empty tail!"


-- A variation that uses a less generic consEither but works w/o deep unions

main' =
      do
         -- Handle the shapes polymorphically
         let scribble = consEither' (rectangle 10 20 5 6)
                            $ consEither' (circle 15 25 8)
                            $ []
         mapM_ ( \x -> 
                   do
                      draw x
                      draw (rMoveTo 100 100 x)
               )
               scribble

         -- Handle rectangle-specific instance
         draw $ setWidth 30 (rectangle 0 0 15 15)


type AllShapes = Either RectangleData CircleData

class ConsEither x 
 where
  consEither' :: x -> [AllShapes] -> [AllShapes]

instance ConsEither RectangleData
 where
  consEither' = (:) . Left

instance ConsEither CircleData
 where
  consEither' = (:) . Right
