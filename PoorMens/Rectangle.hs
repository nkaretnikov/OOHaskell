{-# OPTIONS -fglasgow-exts #-}

module Rectangle where

import Shape
import Subtype

data Rectangle =
     Rectangle { rectangle2shape :: Shape
               , getWidth  :: Int 
               , getHeight :: Int 
               }
 
-- Substantiate the subtyping relation
instance Subtype Rectangle Shape
 where
  f .?. a = f $ rectangle2shape $ a
  f .!. a = a { rectangle2shape = f $ rectangle2shape a }

-- Setters
setWidth :: Subtype a Rectangle => Int -> a -> a
setWidth i = (.!.) (\s -> s { getWidth = i} )

setHeight :: Subtype a Rectangle => Int -> a -> a
setHeight i = (.!.) (\s -> s { getHeight = i} )

-- Implement abstract draw method
instance Draw Rectangle
 where
  draw a =  putStrLn ("Drawing a Rectangle at:("
         ++ (show (getX .?. a))
         ++ ","
         ++ (show (getY .?. a))
         ++ "), width " ++ (show (getWidth .?. a))
         ++ ", height " ++ (show (getHeight .?. a)))
