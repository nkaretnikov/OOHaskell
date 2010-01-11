
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The composed type of circles

data CircleData =
     CircleData { valShape  :: ShapeData
                 , valRadius :: Int
                 }


-- A "closed" constructor

circle x y r
 = CircleData { valShape  = shape x y
              , valRadius = r
              }


-- A circle is a shape

instance Shape CircleData
 where
  readShape f    = f . valShape
  writeShape f s = s { valShape = readShape f s } 
  draw s
    =  putStrLn ("Drawing a Circle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), radius "
    ++ (show (getRadius s)))


-- The circle interface

class Shape s => Circle s
 where
  readCircle  :: (CircleData -> t)         -> s -> t
  writeCircle :: (CircleData -> CircleData) -> s -> s
  getRadius   :: s -> Int
  getRadius   =  readCircle valRadius
  setRadius   :: Int -> s -> s
  setRadius i =  writeCircle (\s -> s  { valRadius = i })


-- A circle is circle is a ...

instance Circle CircleData
 where
  readCircle  = id
  writeCircle = id
