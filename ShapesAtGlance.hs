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


module Shapes where

import OOHaskell
import Label5

-- The class Shape

-- First, declare the labels.

data GetX    = GetX
data GetY    = GetY
data SetX    = SetX
data SetY    = SetY
data MoveTo  = MoveTo
data RMoveTo = RMoveTo
data Draw    = Draw -- Needed in concrete subclasses


-- Note that unlike C++ version, our class is polymorphic in x and y.
-- Those fields can be any Num.
-- The fields are private, just as they are in C++ code

shape x y self
  = do
       xRef <- newIORef x
       yRef <- newIORef y
       return $
            GetX     .=. readIORef xRef
        .*. GetY     .=. readIORef yRef
        .*. SetX     .=. writeIORef xRef
        .*. SetY     .=. writeIORef yRef
        .*. MoveTo   .=. (\x y -> do (self # SetX) x; (self # SetY) y)
        .*. RMoveTo  .=. (\dx dy ->
              do
                 x  <- self # GetX
                 y  <- self # GetY
                 (self # MoveTo) (x + dx) (y + dy))
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
            GetWidth  .=. readIORef widthRef
        .*. GetHeight .=. readIORef heightRef
        .*. SetWidth  .=. writeIORef widthRef
        .*. SetHeight .=. writeIORef heightRef
        .*. Draw      .=. 
              putStr  "Drawing a Rectangle at:(" <<
                      self # GetX << ls "," << self # GetY <<
                      ls "), width " << self # GetWidth <<
                      ls ", height " << self # GetHeight <<
                      ls "\n"
        .*. super

data GetWidth  = GetWidth
data GetHeight = GetHeight
data SetWidth  = SetWidth
data SetHeight = SetHeight


-- Circle: inherits from Shape

circle x y radius self
  = do
       super <- shape x y self
       radiusRef <- newIORef radius
       return $
            GetRadius  .=. readIORef radiusRef
        .*. SetRadius  .=. writeIORef radiusRef
        .*. Draw       .=. 
              putStr  "Drawing a Circle at:(" <<
                      self # GetX << ls "," << self # GetY <<
                      ls "), radius " << self # GetRadius <<
                      ls "\n"
        .*. super

data GetRadius = GetRadius
data SetRadius = SetRadius


-- Weirich's / Rathman's test case

main =
  do
       s1 <- mfix $ rectangle 10 20 5 6
       s2 <- mfix $ circle 15 25 8
       let scribble = cons s1 (cons s2 nil)
       mapM_ (\x -> do
                       x # Draw
                       (x # RMoveTo) 100 100
                       x # Draw)
             scribble

      -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # SetWidth $ 30
--       arec # SetRadius $ 40
       arec # Draw
