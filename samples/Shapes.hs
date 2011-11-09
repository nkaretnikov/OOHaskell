{-# LANGUAGE EmptyDataDecls, DeriveDataTypeable, TemplateHaskell #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

{-

OOHaskell (C) 2004--2007, Oleg Kiselyov, Ralf Laemmel

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html

-}


module Shapes where

import OOHaskell

-- The class Shape

-- First, declare the labels.

$(label "getX")
$(label "getY")
$(label "setX")
$(label "setY")
$(label "moveTo")
$(label "moveBy")
$(label "draw")


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
        .*. moveBy   .=. (\dx dy ->
              do
                 x  <- self # getX
                 y  <- self # getY
                 (self # moveTo) (x + dx) (y + dy))
        .*. emptyRecord


-- More labels

$(label "getWidth")
$(label "getHeight")
$(label "setWidth")
$(label "setHeight")


-- Rectange: inherits from Shape

rectangle x y width height self
  = do
       super     <- shape x y self
       widthRef  <- newIORef width
       heightRef <- newIORef height
       return $
            getWidth  .=. readIORef  widthRef
        .*. getHeight .=. readIORef  heightRef
        .*. setWidth  .=. writeIORef widthRef
        .*. setHeight .=. writeIORef heightRef
        .*. draw      .=. printLn ("Drawing a Rectangle at:("
                               << self # getX << "," << self # getY
                               << "), width " << self # getWidth
                               << ", height " << self # getHeight)
        .*. super


-- More labels

$(label "getRadius")
$(label "setRadius")


-- Circle: inherits from Shape

circle x y radius self
  = do
       super     <- shape x y self
       radiusRef <- newIORef radius
       return $
            getRadius  .=. readIORef radiusRef
        .*. setRadius  .=. writeIORef radiusRef
        .*. draw       .=. printLn ("Drawing a Circle at:("
                                << self # getX << "," << self # getY
                                << "), radius " << self # getRadius)
        .*. super


-- Test case for heterogeneous collections

main = do
          -- Construct a list of shapes
          s1 <- mfix $ rectangle 10 20 5 6
          s2 <- mfix $ circle 15 25 8
          let scribble = cons s1 (cons s2 nil)

          -- Handle the shapes in the list polymorphically       
          mapM_ (\s -> do
                          s # draw
                          (s # moveBy) 100 100
                          s # draw)
                scribble

          -- call a rectangle specific function
          arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
          arec # setWidth $ 30
--          arec # setRadius $ 40
          arec # draw
