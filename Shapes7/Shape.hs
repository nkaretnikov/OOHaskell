
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where


-- Data of shapes

data ShapeData =
     ShapeData { getX :: Int
               , getY :: Int }


-- Constructor for shapes

shape x y = ShapeData { getX = x
                      , getY = y }


-- The shape interface

class Shape s
 where
  moveTo     :: Int -> Int -> s -> s
  rMoveTo    :: Int -> Int -> s -> s
  draw       :: s -> IO ()

moveTo' x y s = shape x y 
rMoveTo' deltax deltay s = moveTo' x y s
  where
     x = getX s + deltax
     y = getY s + deltay
