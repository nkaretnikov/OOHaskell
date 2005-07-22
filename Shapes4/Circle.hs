
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Data.IORef
import Shape


-- The delta of circles

data CircleDelta w =
     CircleDelta { getRadius'  :: IO Int
                 , setRadius'  :: Int -> IO ()
                 , circleTail  :: w
                 }


-- An extension of Shape

type Circle w = Shape (CircleDelta w)


-- Closed constructor for circles

circle x y r
  = shape x y drawCircle shapeTail
 where

  drawCircle self
    =  
       putStr  "Drawing a Circle at:(" <<
       getX self << ls "," << getY self <<
       ls "), radius " << getRadius self <<
       ls "\n"

  shapeTail 
    = do 
         rRef <- newIORef r
         return ( \self -> 
            CircleDelta
                { getRadius' = readIORef rRef
                , setRadius' = \r' ->  writeIORef rRef r'
                , circleTail = ()
                } )


-- Hide nested position of rectangle accessors

getRadius = getRadius'  . shapeTail
setRadius = setRadius'  . shapeTail
