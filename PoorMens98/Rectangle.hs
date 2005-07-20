module Rectangle where

import Shape


-- The delta of rectangles
data RectangleDelta =
     RectangleDelta { getWidth     :: Int 
                    , getHeight    :: Int
                    }


-- An extension of Shape
type Rectangle = Shape RectangleDelta


-- A "closed" constructor
rectangle x y w h
 = shape x y $ RectangleDelta {
                 getWidth = w
               , getHeight = h
               }


-- Setters
setHeight :: Int -> Rectangle -> Rectangle
setHeight i s
 = s { rest = (rest s) { getHeight = i } }

setWidth :: Int -> Rectangle -> Rectangle
setWidth i s
 = s { rest = (rest s) { getWidth = i } }


-- Implement abstract draw method
instance Draw RectangleDelta
 where
  draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), width "
    ++ (show (getWidth (rest s)))
    ++ ", height "
    ++ (show (getHeight (rest s))))
