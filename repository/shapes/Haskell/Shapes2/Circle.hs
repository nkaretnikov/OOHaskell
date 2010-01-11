
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The delta of circles

data CircleDelta w =
     CircleDelta { getRadius  :: Int
                 , circleTail :: w }


-- An extension of Shape

type Circle w = Shape (CircleDelta w)


-- A "closed" constructor

circle x y radius
 = shape x y $ CircleDelta { getRadius  = radius
                           , circleTail = () }


-- Setter

setRadius :: Int -> Circle w -> Circle w
setRadius i s = s { shapeTail = (shapeTail s) { getRadius = i } }


-- Implement abstract draw method

instance Draw (CircleDelta w) where
  draw s
    =  putStrLn $ concat ["Drawing a Circle at:", show (getX s,getY s), 
                          ", radius ", show (getRadius (shapeTail s))]

