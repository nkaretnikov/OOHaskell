
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Shape


-- The composed type of rectangles

data RectangleData =
     RectangleData { valShape  :: ShapeData
                   , valWidth  :: Int 
                   , valHeight :: Int
                   }


-- A "closed" constructor

rectangle x y w h
 = RectangleData { valShape  = shape x y
                 , valWidth  = w
                 , valHeight = h
                 }


-- A rectangle is a shape

instance Shape RectangleData
 where
  readShape f    = f . valShape
  writeShape f s = s { valShape = readShape f s } 
  draw s
    =   putStrLn ("Drawing a Rectangle at:("
    ++ (show (getX s))
    ++ ","
    ++ (show (getY s))
    ++ "), width "
    ++ (show (getWidth s))
    ++ ", height "
    ++ (show (getHeight s)))


-- The rectangle interface

class Shape s => Rectangle s
 where
  readRectangle  :: (RectangleData -> t)         -> s -> t
  writeRectangle :: (RectangleData -> RectangleData) -> s -> s
  getWidth       :: s -> Int
  getWidth       =  readRectangle valWidth
  setWidth       :: Int -> s -> s
  setWidth i     =  writeRectangle (\s -> s  { valWidth = i })
  getHeight      :: s -> Int
  getHeight      =  readRectangle valHeight
  setHeight      :: Int -> s -> s    
  setHeight i    =  writeRectangle (\s -> s  { valHeight = i })


-- A rectangle is rectangle is a ...

instance Rectangle RectangleData
 where
  readRectangle  = id
  writeRectangle = id
