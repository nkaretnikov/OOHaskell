
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape


-- The delta for rectangles

data RectangleDelta w =
     RectangleDelta { getWidth'     :: Int 
                    , getHeight'    :: Int
                    , setWidth'     :: Int -> Rectangle w
                    , setHeight'    :: Int -> Rectangle w
                    , rectangleTail :: w
                    }


-- An extension of Shape

type Rectangle w = Shape (RectangleDelta w)


-- Closed constructor for rectangles

rectangle x y w h
  = shape x y drawRectangle shapeTail

 where

  drawRectangle x y
    =  putStrLn ("Drawing a Rectangle at:("
    ++ (show x)
    ++ ","
    ++ (show y)
    ++ "), width "
    ++ (show w)
    ++ ", height "
    ++ (show h) )

  shapeTail 
    = RectangleDelta { getWidth'     = w 
                     , getHeight'    = h
                     , setWidth'     = \w' -> rectangle x y w' h
                     , setHeight'    = \h' -> rectangle x y w h'
                     , rectangleTail = ()
                     }


-- Hide nested position of rectangle accessors

getWidth  = getWidth'  . shapeTail
getHeight = getHeight' . shapeTail
setWidth  = setWidth'  . shapeTail
setHeight = setHeight' . shapeTail
