
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The composed type of circles

data CircleData =
     CircleData { getShape :: ShapeData
                 , getRadius :: Int
                 }


-- A "closed" constructor

circle x y r
 = CircleData { getShape  = shape x y
              , getRadius = r
              }


-- A circle is a shape

instance Shape CircleData
 where
  moveTo x y s = s { getShape = moveTo' x y (getShape s) }
  rMoveTo deltax deltay s = s { getShape = rMoveTo' deltax deltay (getShape s) }
  draw s
    =  putStrLn ("Drawing a Circle at:("
    ++ (show (getX (getShape s)))
    ++ ","
    ++ (show (getY (getShape s)))
    ++ "), radius "
    ++ (show (getRadius s)))
