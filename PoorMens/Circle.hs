{-# OPTIONS -fglasgow-exts #-}

module Circle where

import Shape
import Subtype

data Circle =
     Circle { circle2shape :: Shape
            , getRadius :: Int 
            }

-- Substantiate the subtyping relation
instance Subtype Circle Shape
 where
  f .?. a = f $ circle2shape $ a
  f .!. a = a { circle2shape = f $ circle2shape a }

-- Setter
setRadius :: Subtype a Circle => Int -> a -> a
setRadius i = (.!.) (\s -> s { getRadius = i} )

-- Implement abstract draw method
instance Draw Circle
 where
  draw a =  putStrLn ("Drawing a Circle at:("
         ++ (show (getX .?. a))
         ++ ","
         ++ (show (getY .?. a))
         ++ "), radius "
         ++ (show (getRadius .?. a)))
