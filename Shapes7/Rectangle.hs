
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape


-- The composed type of rectangles

data RectangleData =
     RectangleData { shapeData  :: ShapeData
                   , widthData  :: Int 
                   , heightData :: Int
                   }


-- A "closed" constructor

rectangle x y w h
 = RectangleData { shapeData  = shape x y
                 , widthData  = w
                 , heightData = h
                 }


-- A rectangle is a shape

instance Shape RectangleData
 where
  moveTo x y s = s { shapeData = moveTo' x y (shapeData s) }
  rMoveTo dx dy s = s { shapeData = rMoveTo' dx dy (shapeData s) }
  draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (xData (shapeData s)))
    ++ ","
    ++ (show (yData (shapeData s)))
    ++ "), width "
    ++ (show (widthData s))
    ++ ", height "
    ++ (show (heightData s)))
