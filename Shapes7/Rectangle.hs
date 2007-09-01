
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape


-- The composed type of rectangles

data RectangleData =
     RectangleData { getShape  :: ShapeData
                   , getWidth  :: Int 
                   , getHeight :: Int
                   }


-- A "closed" constructor

rectangle x y w h
 = RectangleData { getShape  = shape x y
                 , getWidth  = w
                 , getHeight = h
                 }


-- A rectangle is a shape

instance Shape RectangleData
 where
  moveTo x y s = s { getShape = moveTo' x y (getShape s) }
  rMoveTo deltax deltay s = s { getShape = rMoveTo' deltax deltay (getShape s) }
  draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (getX (getShape s)))
    ++ ","
    ++ (show (getY (getShape s)))
    ++ "), width "
    ++ (show (getWidth s))
    ++ ", height "
    ++ (show (getHeight s)))
