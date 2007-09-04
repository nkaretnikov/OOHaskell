{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- Shapes with existentials instead of typeclasses

module Shape where


-- Data of shapes

data ShapeData =
     ShapeData { xData :: Int
               , yData :: Int }


-- Constructor for shapes

shape_data x y = ShapeData { xData = x
			   , yData = y }


-- The shape interface

-- interface with the explicit type of private data
data ShapeR shapedata = 
    ShapeR{ sh_data :: shapedata,
	    sh_moveTo  :: Int -> Int -> shapedata -> shapedata,
	    sh_rMoveTo :: Int -> Int -> shapedata -> shapedata,
	    sh_draw    :: shapedata -> IO () }

-- transform private state
xf_shapeR sr fn = sr {sh_data = fn (sh_data sr)}

-- Interface with the abstracted data
data Shape = forall shapedata . Shape (ShapeR shapedata)


-- Public interface of shapes
moveTo :: Int -> Int -> Shape -> Shape
moveTo x y (Shape sr)    = Shape (xf_shapeR sr (sh_moveTo sr x y))

rMoveTo :: Int -> Int -> Shape -> Shape
rMoveTo dx dy (Shape sr) = Shape (xf_shapeR sr (sh_rMoveTo sr dx dy))

draw :: Shape -> IO ()
draw (Shape sr) = sh_draw sr (sh_data sr)


-- Auxiliary
moveTo' x y s = shape_data x y 
rMoveTo' deltax deltay s = moveTo' x y s
  where
    x = xData s + deltax
    y = yData s + deltay
