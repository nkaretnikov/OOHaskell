
-- (C) 2004-2007, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where


-- Data of shapes

data ShapeData =
     ShapeData { valX :: Int
               , valY :: Int }


-- Constructor for shapes

shape x y = ShapeData { valX = x
                      , valY = y }


-- The shape interface

class Shape s where
  readShape  :: (ShapeData -> t)         -> s -> t
  writeShape :: (ShapeData -> ShapeData) -> s -> s
  getX, getY :: s -> Int
  getX       =  readShape valX
  getY       =  readShape valY
  setX, setY :: Int -> s -> s
  setX i     =  writeShape (\s -> s  { valX = i })
  setY i     =  writeShape (\s -> s  { valY = i })
  moveTo     :: Int -> Int -> s -> s
  moveTo x y =  setY y . setX x 
  rMoveTo    :: Int -> Int -> s -> s
  rMoveTo deltax deltay s = moveTo x y s
   where x = getX s + deltax
	 y = getY s + deltay
  draw       :: s -> IO ()
