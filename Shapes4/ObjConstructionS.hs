
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system


-- Staging problems with object conctrsution 

module ObjConstructionS where

import Data.IORef
import Shape hiding (shape)
import Circle hiding (circle)
import SMFix

-- Constructor for shapes
-- Hardly any changes
shape x y concreteDraw tail self
  = do
       xRef  <- newIORef x
       yRef  <- newIORef y
       tail' <- tail
       -- z <- getX self Again, the type error
       sret self (\self -> Shape
                { getX      = readIORef xRef
                , getY      = readIORef yRef
                , setX      = \x' -> writeIORef xRef x'
                , setY      = \y' -> writeIORef yRef y'
                , moveTo    = \x' y' -> do { setX self x'; setY self y' }
                , rMoveTo   = \deltax deltay -> 
                                do
                                   x <- getX self
                                   y <- getY self
                                   moveTo self (x+deltax) (y+deltay)
                , draw      = concreteDraw self
                , shapeTail = tail' self
                })


-- Closed constructor for circles

-- What can go wrong
circle x y r self
  = do
    -- x' <- getX self -- the object isn't constructed yet
    -- Now, that gives a type error!
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
		  seq self  -- safe
		  $ seq (getX self) -- safe
            CircleDelta
                { getRadius' = readIORef rRef
                , setRadius' = \r' ->  writeIORef rRef r'
                , circleTail = ()
                } )


test = 
      do
      s <- smfix $ circle 15 25 8
      draw s


