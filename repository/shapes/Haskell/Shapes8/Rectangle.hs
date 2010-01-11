
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



-- Concrete rectangles

rectangle x y w h = ShapeC { getData = RectangleData (ShapeData x y) w h
                           , moveToI  = moveTo
                           , rMoveToI = rMoveTo
                           , drawI    = draw }

 where
  moveTo x y s    = updData s (moveTo' x y)
  rMoveTo dx dy s = updData s (rMoveTo' dx dy)
  updData s f = s { shapeData = f (shapeData s) }
  draw s =   putStrLn ("Drawing a Rectangle at:("
         ++ (show (xData (shapeData s)))
         ++ ","
         ++ (show (yData (shapeData s)))
         ++ "), width "
         ++ (show (widthData s))
         ++ ", height "
         ++ (show (heightData s)))

-- rectangle-specific operation

setWidth :: Int -> ShapeC RectangleData -> ShapeC RectangleData
setWidth w s = s { getData = (getData s) { widthData = w } }
