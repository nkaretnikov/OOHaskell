module Circle where

import Shape


-- The delta of circles
data CircleDelta =
     CircleDelta { getRadius :: Int }


-- An extension of Shape
type Circle = Shape CircleDelta


-- A "closed" constructor
circle x y r
 = shape x y $ CircleDelta { getRadius = r }


-- Setter
setRadius :: Int -> Circle -> Circle
setRadius i s = s { rest = (rest s) { getRadius = i } }


-- Implement abstract draw method
instance Draw CircleDelta
 where
  draw s
    =  putStrLn ("Drawing a Circle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), radius "
    ++ (show (getRadius (rest s))))
