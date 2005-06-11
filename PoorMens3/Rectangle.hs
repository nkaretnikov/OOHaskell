{-# OPTIONS -fglasgow-exts #-}

module Rectangle where

import Shape

-- The delta of rectangles
data Rectanglish w =
     Rectanglish { getWidth     :: Int 
                 , getHeight    :: Int
                 , rectangleExt :: w }

-- An extension of Shape
type Rectangle w = Shape (Rectanglish w)

-- A "closed" constructor
rectangle x y w h
 = shape x y $ Rectanglish {
                 getWidth = w
               , getHeight = h
               , rectangleExt = () }

-- Setters
setHeight :: Int -> Rectangle w -> Rectangle w
setHeight i s
 = s { shapeExt = (shapeExt s) { getHeight = i } }

setWidth :: Int -> Rectangle w -> Rectangle w
setWidth i s
 = s { shapeExt = (shapeExt s) { getWidth = i } }

-- Implement abstract draw method
instance Draw (Rectanglish w)
 where
  draw s =
        putStrLn ("Drawing a Rectangle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), width "
    ++ (show (getWidth (shapeExt s)))
    ++ ", height "
    ++ (show (getHeight (shapeExt s))))
