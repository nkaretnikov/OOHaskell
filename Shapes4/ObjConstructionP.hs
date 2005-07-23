
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system


-- Staging problems with object conctrsution 

module ObjConstructionP where

import Data.IORef
import Shape
import Circle
import Control.Monad.Fix



-- Closed constructor for circles

-- What can go wrong
circle_bad x y r self
  = do
    x' <- getX self -- the object isn't constructed yet
    s <- shape x y drawCircle shapeTail self
    return s
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

-- Running it loops
test_bad = 
      do
      s <- mfix $ circle_bad 15 25 8
      draw s
