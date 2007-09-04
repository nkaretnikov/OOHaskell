
-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The composed type of circles

data CircleData =
     CircleData { shapeData :: ShapeData
                 , radiusData :: Int }


-- A "closed" constructor

circle_data x y r = CircleData { shapeData  = shape_data x y
			       , radiusData = r }


-- A circle is a shape

circle x y r = ShapeR {sh_data    = cdata,
		       sh_moveTo  = moveTo,
		       sh_rMoveTo = rMoveTo,
		       sh_draw    = draw}
 where
 cdata  = circle_data x y r
 moveTo x y s    = s{ shapeData = moveTo' x y (shapeData s) }
 rMoveTo dx dy s = s{ shapeData = rMoveTo' dx dy (shapeData s) }
 draw s
    =  putStrLn ("Drawing a Circle at:("
    ++ (show (xData (shapeData s)))
    ++ ","
    ++ (show (yData (shapeData s)))
    ++ "), radius "
    ++ (show (radiusData s)))


-- An interface in case more kinds of circles show up
-- Extensibility is problematic without the extensible records or typeclasses..

data CircleR cdata = CircleR{c_shape :: ShapeR cdata,
			     getRadius :: cdata -> Int,
			     setRadius :: Int -> cdata -> cdata}
