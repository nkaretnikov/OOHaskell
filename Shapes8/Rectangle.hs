
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

rectangle_data x y w h
 = RectangleData { shapeData  = shape_data x y
                 , widthData  = w
                 , heightData = h
                 }

-- A rectangle is a shape

rectangle x y w h = ShapeR {sh_data    = rdata,
			    sh_moveTo  = moveTo,
			    sh_rMoveTo = rMoveTo,
			    sh_draw    = draw}
 where
 rdata  = rectangle_data x y w h
 moveTo x y s    = s{ shapeData = moveTo' x y (shapeData s) }
 rMoveTo dx dy s = s{ shapeData = rMoveTo' dx dy (shapeData s) }
 draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (xData (shapeData s)))
    ++ ","
    ++ (show (yData (shapeData s)))
    ++ "), width "
    ++ (show (widthData s))
    ++ ", height "
    ++ (show (heightData s)))

-- rectangle-specific operation

set_width :: Int -> ShapeR RectangleData -> ShapeR RectangleData
set_width w s = s{ sh_data = (sh_data s) {widthData = w} }



