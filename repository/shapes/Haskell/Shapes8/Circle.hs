
-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The composed type of circles

data CircleData =
     CircleData { shapeData  :: ShapeData
                , radiusData :: Int }


-- Concrete circles

circle x y r = ShapeC { getData  = CircleData (ShapeData x y) r
                      , moveToI  = moveTo
                      , rMoveToI = rMoveTo
		      , drawI    = draw }
 where
  moveTo x y s    = updData s (moveTo' x y)
  rMoveTo dx dy s = updData s (rMoveTo' dx dy)
  updData s f = s { shapeData = f (shapeData s) }
  draw s =  putStrLn ("Drawing a Circle at:("
         ++ (show (xData (shapeData s)))
         ++ ","
         ++ (show (yData (shapeData s)))
         ++ "), radius "
         ++ (show (radiusData s)))
