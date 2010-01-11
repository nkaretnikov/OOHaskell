
-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where


-- Data of shapes

data ShapeData =
     ShapeData { xData :: Int
               , yData :: Int }


-- Constructor for shapes

shape x y = ShapeData { xData = x
                      , yData = y }


-- The shape interface

class Shape s where
  moveTo     :: Int -> Int -> s -> s
  rMoveTo    :: Int -> Int -> s -> s
  draw       :: s -> IO ()

moveTo' x y s = shape x y 
rMoveTo' dx dy s = moveTo' x y s
  where x = xData s + dx
	y = yData s + dy
