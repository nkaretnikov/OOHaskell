
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where

-- Data of extensible shapes

data Shape w =
     Shape { getX :: Int
           , getY :: Int
           , shapeTail :: w }


-- Constructor for shapes

shape x y w = Shape { getX = x
                    , getY = y
                    , shapeTail = w }


-- Reusable functionality on shapes
-- (These cannot be overridden.)

setX :: Int -> Shape w -> Shape w
setX i s = s { getX = i }

setY :: Int -> Shape w -> Shape w
setY i s = s { getY = i }

moveTo :: Int -> Int -> Shape w -> Shape w
moveTo x y = setY y . setX x 

rMoveTo :: Int -> Int -> Shape w -> Shape w
rMoveTo deltax deltay s = moveTo x y s
  where
    x = getX s + deltax
    y = getY s + deltay


-- A class for type-specific drawing
-- (Virtual methods are generally mapped to such classes.)

class Draw w
 where
  draw :: Shape w -> IO ()
