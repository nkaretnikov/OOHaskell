{-# OPTIONS -fglasgow-exts #-}

module Shape where

import Subtype


-- Mutable data of shapes
data Shape =
     Shape { getX    :: Int
           , getY    :: Int
           }

-- Setters
setX :: Subtype a Shape => Int -> a -> a
setX i = (.!.) (\s -> s { getX = i} )

setY :: Subtype a Shape => Int -> a -> a
setY i = (.!.) (\s -> s { getY = i} )

-- Move methods on shapes
moveTo :: Subtype a Shape => Int -> Int -> a -> a
moveTo x y = setY y . setX x 

rMoveTo :: Subtype a Shape => Int -> Int -> a -> a
rMoveTo deltax deltay a = moveTo x y a
 where
  x = getX .?. a + deltax
  y = getY .?. a + deltay


-- The abstract method for drawing shapes
class Subtype a Shape => Draw a
 where
  draw :: a -> IO()
