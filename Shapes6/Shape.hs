{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Shape where

import Subtype
import GHC.IOBase


-- Data of shapes

data Shape =
     Shape { getX    :: Int
           , getY    :: Int
           }


-- Setters

setX :: (Subtype a Shape, Start a) => Int -> a -> IO a
setX i a = do 
              start a 
              (\s -> returnIO $ s { getX = i} ) .!>. a

setY :: (Subtype a Shape, Start a) => Int -> a -> IO a
setY i a = do 
              start a 
              (\s -> returnIO $ s { getY = i} ) .!>. a


-- Move methods on shapes

moveTo :: (Subtype a Shape, Start a) => Int -> Int -> a -> IO a
moveTo x y a = do a' <- setY y a; setX x a'

rMoveTo :: (Subtype a Shape, Start a) => Int -> Int -> a -> IO a
rMoveTo deltax deltay a =
  do
     x <- (returnIO . getX) .?. a
     y <- (returnIO . getY) .?. a
     moveTo (x + deltax) (y + deltay) a


-- The abstract method for drawing shapes

class Subtype a Shape => Draw a
 where
  draw :: a -> IO ()


-- Another abstract method for update transactions

class Subtype a Shape => Start a
 where
  start :: a -> IO ()
