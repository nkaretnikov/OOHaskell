{-# OPTIONS -fglasgow-exts #-}

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Shape
import Subtype
import GHC.IOBase


-- The record type for circle data

data Circle =
     Circle { circle2shape :: Shape
            , getRadius :: Int 
            }


-- Substantiate the subtyping relation

instance Subtype Circle Shape
 where
  f .?>. a = f $ circle2shape a
  f .!>. a = (f .?>. a) >>= (\x -> returnIO a { circle2shape = x })


-- Setter

setRadius :: (Subtype a Circle, Start a) => Int -> a -> IO a
setRadius i a = do 
                  start a 
                  (\s -> returnIO $ s { getRadius = i} ) .!>. a


-- Implement abstract methods

instance Draw Circle
 where
  draw a = do
              x <- ((returnIO . getX)       .?>. a)
              y <- ((returnIO . getY)       .?>. a)
              r <- ((returnIO . getRadius)  .?>. a)
              putStrLn ("Drawing a Circle at:("
                ++ (show x)
                ++ ","
                ++ (show y)
                ++ "), radius "
                ++ (show r))

instance Start Circle
 where
  start _ = putStrLn "Begin updating circle ..."
