
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module ConsEither where

import Shape
import Circle
import Rectangle
import Polymorph hiding (main)


-- Constructors for lists of shapes
nil :: [AnyShape]
nil = []

class Cons s
 where
  cons :: s -> [AnyShape] -> [AnyShape]

instance Cons RectangleData
 where
  cons = (:) . Left

instance Cons CircleData
 where
  cons = (:) . Right


-- Weirich's / Rathman's test case

main =
      do
           -- Handle the shapes polymorphically
           let scribble = cons (rectangle 10 20 5 6)
                              $ cons (circle 15 25 8) nil
           mapM_ ( \x -> 
                    do
                       draw x
                       draw (rMoveTo 100 100 x) )
                 scribble

           -- Handle rectangle-specific instance
           let r = rectangle 0 0 15 15
           let r' = r { widthData = 30 } 
           draw r' 
