
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape


-- The delta of rectangles

data RectangleDelta w =
     RectangleDelta { getWidth      :: Int 
                    , getHeight     :: Int
                    , rectangleTail :: w }


-- An extension of Shape

type Rectangle w = Shape (RectangleDelta w)


-- A "closed" constructor

rectangle x y w h
 = shape x y $ RectangleDelta {
                 getWidth      = w
               , getHeight     = h
               , rectangleTail = () }


-- Setters

setHeight :: Int -> Rectangle w -> Rectangle w
setHeight i s = s { shapeTail = (shapeTail s) { getHeight = i } }

setWidth :: Int -> Rectangle w -> Rectangle w
setWidth i s = s { shapeTail = (shapeTail s) { getWidth = i } }


-- Implement abstract draw method

instance Draw (RectangleDelta w)
 where
  draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), width "
    ++ (show (getWidth (shapeTail s)))
    ++ ", height "
    ++ (show (getHeight (shapeTail s))))
