
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape


-- The delta of circles

data CircleDelta w =
     CircleDelta { getRadius'  :: Int
                 , setRadius'  :: Int -> Circle w
                 , circleTail  :: w
                 }


-- An extension of Shape

type Circle w = Shape (CircleDelta w)


-- Closed constructor for circles

circle x y r
  = shape x y drawCircle shapeTail

 where

  drawCircle x y
    =  putStrLn ("Drawing a Circle at:("
    ++ (show x)
    ++ ","
    ++ (show y)
    ++ "), radius "
    ++ (show r) )

  shapeTail 
    = CircleDelta { getRadius' = r
                  , setRadius' = \r' -> circle x y r'
                  , circleTail = ()
                  }


-- Hide nested position of rectangle accessors

getRadius = getRadius'  . shapeTail
setRadius = setRadius'  . shapeTail
