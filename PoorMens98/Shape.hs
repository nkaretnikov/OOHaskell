module Shape where

-- Mutable data of extensible shapes
data Shape w =
     Shape { getX :: Int
           , getY :: Int
           , rest :: w }

-- Constructor for shapes
shape x y w = Shape { getX = x
                    , getY = y
                    , rest = w }


-- Reusable functionality on shapes

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

class Draw w
 where
  draw :: Shape w -> IO ()



{-

-- Mutable data of extensible shapes
data Shape w =
     Shape { xVal :: Int
           , yVal :: Int
           , rest :: w }


-- Constructor for shapes
shape x y w = Shape { xVal = x
                    , yVal = y
                   , rest = w }


-- All reusable functions on shapes

class IShape s
 where
  getX    :: s -> Int
  setX    :: Int -> s -> s
  getY    :: s -> Int
  setY    :: Int -> s -> s  
  moveTo  :: Int -> Int -> s -> s
  rMoveTo :: Int -> Int -> s -> s
  draw    :: s -> IO ()

instance IShapeDraw w => IShape (Shape w)
 where
  getX = xVal
  setX i s = s { xVal = i }
  getY = yVal
  setY i s = s { yVal = i }
  moveTo x y = setY y . setX x 
  rMoveTo deltax deltay s = moveTo x y s
   where
    x = getX s + deltax
    y = getY s + deltay
  draw = shapeDraw


-- A helper class to discriminate on Shape extensions

class IShapeDraw w
 where
  shapeDraw :: Shape w -> IO ()

-}
