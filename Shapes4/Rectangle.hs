
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Data.IORef
import Shape


-- The delta for rectangles

data RectangleDelta w =
     RectangleDelta { getWidth'     :: IO Int 
                    , getHeight'    :: IO Int
                    , setWidth'     :: Int -> IO ()
                    , setHeight'    :: Int -> IO ()
                    , rectangleTail :: w
                    }


-- An extension of Shape

type Rectangle w = Shape (RectangleDelta w)


-- Closed constructor for rectangles

rectangle x y w h = shape x y drawRectangle shapeTail
 where

  drawRectangle s
    =  
       putStr "Drawing a Rectangle at:(" <<
       getX s << ls "," << getY s <<
       ls "), width " << getWidth s <<
       ls ", height " << getHeight s <<
       ls "\n"

  shapeTail
    = do 
         wRef <- newIORef w
         hRef <- newIORef h
         return (\s -> 
            RectangleDelta
                { getWidth'     = readIORef wRef 
                , getHeight'    = readIORef hRef
                , setWidth'     = \w' -> writeIORef wRef w'
                , setHeight'    = \h' -> writeIORef hRef h'
                , rectangleTail = ()
                } )


-- Hide nested position of rectangle accessors

getWidth  = getWidth'  . shapeTail
getHeight = getHeight' . shapeTail
setWidth  = setWidth'  . shapeTail
setHeight = setHeight' . shapeTail
