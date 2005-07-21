{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape
import Subtype
import GHC.IOBase


-- The record type for rectangle data

data Rectangle =
     Rectangle { rectangle2shape :: Shape
               , getWidth  :: Int 
               , getHeight :: Int 
               }

 
-- Substantiate the subtyping relation

instance Subtype Rectangle Shape
 where
  f .?>. a = f $ rectangle2shape a
  f .!>. a = (f .?>. a) >>= (\x -> returnIO a { rectangle2shape = x })


-- Setters

setWidth :: (Subtype a Rectangle, Start a) => Int -> a -> IO a
setWidth i a = do 
                  start a 
                  (\s -> returnIO $ s { getWidth = i} ) .!>. a

setHeight :: (Subtype a Rectangle, Start a) => Int -> a -> IO a
setHeight i a = do 
                  start a 
                  (\s -> returnIO $ s { getHeight = i} ) .!>. a


-- Implement abstract methods

instance Draw Rectangle
 where
  draw a = do
              x <- ((returnIO . getX)      .?>. a)
              y <- ((returnIO . getY)      .?>. a)
              w <- ((returnIO . getWidth)  .?>. a)
              h <- ((returnIO . getHeight) .?>. a)
              putStrLn ("Drawing a Rectangle at:("
                ++ (show x)
                ++ ","
                ++ (show y)
                ++ "), width " ++ (show w)
                ++ ", height " ++ (show h))

instance Start Rectangle
 where
  start _ = putStrLn "Begin updating rectangle ..."
