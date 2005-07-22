
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Rectangle where

import Control.Monad.Fix
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

rectangle x y w h
  = mfix $ shape x y drawRectangle shapeTail
 where

  drawRectangle self
    =  
       putStr "Drawing a Rectangle at:(" <<
       getX self << ls "," << getY self <<
       ls "), width " << getWidth self <<
       ls ", height " << getHeight self <<
       ls "\n"

  shapeTail
    = do 
         wRef <- newIORef w
         hRef <- newIORef h
         return ( \self -> 
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
