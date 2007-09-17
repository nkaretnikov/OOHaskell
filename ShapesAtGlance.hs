{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004--2007, Oleg Kiselyov, Ralf Laemmel

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html

-}


module ShapesAtGlance where

import OOHaskell
import Label5

-- The class Shape

-- First, declare the labels.

data GetX;    getX =    undefined::GetX
data GetY;    getY =    undefined::GetY
data SetX;    setX =    undefined::SetX
data SetY;    setY =    undefined::SetY
data MoveTo;  moveTo =  undefined::MoveTo
data RMoveTo; rMoveTo = undefined::RMoveTo
data Draw;    draw =    undefined::Draw -- Needed in concrete subclasses


-- Note that unlike C++ version, our class is polymorphic in x and y.
-- Those fields can be any Num.
-- The fields are private, just as they are in C++ code

shape x y self
  = do
       xRef <- newIORef x
       yRef <- newIORef y
       return $
            getX     .=. readIORef xRef
        .*. getY     .=. readIORef yRef
        .*. setX     .=. writeIORef xRef
        .*. setY     .=. writeIORef yRef
        .*. moveTo   .=. (\x y -> do (self # setX) x; (self # setY) y)
        .*. rMoveTo  .=. (\dx dy ->
              do
                 x  <- self # getX
                 y  <- self # getY
                 (self # moveTo) (x + dx) (y + dy))
        .*. emptyRecord


-- Helpers for C++-like daisy chaining

infixl 7 <<
a << m = a >> (m >>= (putStr . show))

newtype LS = LS String
ls = return . LS
instance Show LS where show (LS x) = x


-- Rectange: inherits from Shape

rectangle x y width height self
  = do
       super <- shape x y self
       widthRef <- newIORef width
       heightRef <- newIORef height
       return $
            getWidth  .=. readIORef widthRef
        .*. getHeight .=. readIORef heightRef
        .*. setWidth  .=. writeIORef widthRef
        .*. setHeight .=. writeIORef heightRef
        .*. draw      .=. 
              putStr  "Drawing a Rectangle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), width " << self # getWidth <<
                      ls ", height " << self # getHeight <<
                      ls "\n"
        .*. super

data GetWidth; getWidth = undefined::GetWidth
data GetHeight; getHeight = undefined::GetHeight
data SetWidth; setWidth = undefined::SetWidth
data SetHeight; setHeight = undefined::SetHeight


-- Circle: inherits from Shape

circle x y radius self
  = do
       super <- shape x y self
       radiusRef <- newIORef radius
       return $
            getRadius  .=. readIORef radiusRef
        .*. setRadius  .=. writeIORef radiusRef
        .*. draw       .=. 
              putStr  "Drawing a Circle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), radius " << self # getRadius <<
                      ls "\n"
        .*. super

data GetRadius; getRadius = undefined::GetRadius
data SetRadius; setRadius = undefined::SetRadius

-- Weirich's / Rathman's test case

main =
  do
       s1 <- mfix $ rectangle 10 20 5 6
       s2 <- mfix $ circle 15 25 8
       let scribble = cons s1 (cons s2 nil)
       mapM_ (\x -> do
                       x # draw
                       (x # rMoveTo) 100 100
                       x # draw)
             scribble

      -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw
