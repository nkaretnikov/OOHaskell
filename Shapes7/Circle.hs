
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The composed type of circles

data CircleData =
     CircleData { shapeData :: ShapeData
                 , radiusData :: Int }


-- A "closed" constructor

circle x y r = CircleData { shapeData  = shape x y
              , radiusData = r }


-- A circle is a shape

instance Shape CircleData where
  moveTo x y s = s { shapeData = moveTo' x y (shapeData s) }
  rMoveTo dx dy s = s { shapeData = rMoveTo' dx dy (shapeData s) }
  draw s
    =  putStrLn $ concat ["Drawing a Circle at:", 
			  show (xData (shapeData s),yData (shapeData s)),
                          ", radius ", show (radiusData s)]


-- An interface in case more kinds of circles show up

class Shape s => Circle s where
  getRadius :: s -> Int
  setRadius :: Int -> s -> s
