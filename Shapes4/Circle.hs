
-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

module Circle where

import Data.IORef
import Shape


-- The delta of circles

data CircleDelta w =
     CircleDelta { getRadius'  :: IO Int
                 , setRadius'  :: Int -> IO ()
                 , circleTail  :: w }


-- An extension of Shape

type Circle w = Shape (CircleDelta w)


-- Closed constructor for circles

circle x y radius = shape x y draw tail
  where
    draw self = putStr "Drawing a Circle at:("
                              << getX self << ls "," << getY self 
                              << ls "), radius " << getRadius self
                              << ls "\n"
    tail = do 
              rRef <- newIORef radius
              return ( \self -> 
                CircleDelta { getRadius' = readIORef rRef
                            , setRadius' = writeIORef rRef
                            , circleTail = () } )


-- Hide nested position of rectangle accessors

getRadius = getRadius'  . shapeTail
setRadius = setRadius'  . shapeTail


-- A variation on circle with logging facilities

circle' x y radius counter self 
  = do
       super <- circle x y radius self
       return super
         { getX = do { tick; getX super }
         , getY = do { tick; getY super } }
 where
   tick = modifyIORef counter ((+) 1)
