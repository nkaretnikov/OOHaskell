{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html

-}


module Shapes where

import OOHaskell


-- The class Shape

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data GetX;     getX     = proxy::Proxy GetX
data GetY;     getY     = proxy::Proxy GetY
data SetX;     setX     = proxy::Proxy SetX
data SetY;     setY     = proxy::Proxy SetY
data MoveTo;   moveTo   = proxy::Proxy MoveTo
data RMoveTo;  rMoveTo  = proxy::Proxy RMoveTo
data Draw;     draw     = proxy::Proxy Draw

-- Note that unlike C++ version, our class is polymorphic in x and y.
-- Those fields can be any Num.
-- The fields are private, just as they are in C++ code

-- This is an optional part in case we want to fix types of virtuals.
-- We need this order unless we dare to see an overlapping pattern warning.

shape x_init y_init self 
  | const False ( narrow self ::
                    Record (  Draw :=: IO ()
                          :*: HNil ) )
  = undefined

-- This is the actual definition

shape x_init y_init self
  = do
      x <- newIORef x_init
      y <- newIORef y_init
      returnIO $
           getX     .=. readIORef x
       .*. getY     .=. readIORef y
       .*. setX     .=. (\newx -> writeIORef x newx)
       .*. setY     .=. (\newy -> writeIORef y newy)
       .*. moveTo   .=. (\newx newy -> do
                                        (self # setX) newx
                                        (self # setY) newy 
                        )
       .*. rMoveTo  .=. (\deltax deltay ->
                 do
                    x  <- self # getX
                    y  <- self # getY
                    (self # moveTo) (x + deltax) (y + deltay)
                        )
       .*. emptyRecord


-- We make the instantiation test.
-- This one should throw *if* the draw method is required.
-- Unfortunately, it does not throw when polymorphism is still present.
-- GHC 6.4 is very lazy when it comes to instance resolution.1

-- complete_shape  
--  = concrete $ shape (1::Int) (2::Int)

{-

-- A sort of type annotation
-- In general, we need explicit eq because of order of structural types

complete_shape = shape
 where
  _ = (mfix (shape (1::Int) (2::Int) . narrow)) :: IO (Shape Int)

-}

{-
    Couldn't match `(:*:) (Proxy Draw, IO ()) HNil' against `HNil'
      Expected type: Shape Int -> IO (Shape Int)
      Inferred type: Record ((:*:) (Proxy GetX, IO Int)
                                   ((:*:) (Proxy GetY, IO Int)
                                          ((:*:) (Proxy SetX, Int -> IO ())
                                                 ...
-}

-- Rectange: inherits from Shape
-- Again, it is polymorphic in the types of its fields

data GetWidth;    getWidth     = proxy::Proxy GetWidth
data GetHeight;   getHeight    = proxy::Proxy GetHeight
data SetWidth;    setWidth     = proxy::Proxy SetWidth
data SetHeight;   setHeight    = proxy::Proxy SetHeight

infixl 7 <<
a << m = a >> (m >>= (putStr . show))

newtype LS = LS String
ls = return . LS
instance Show LS where show (LS x) = x

rectangle x y width height self
  = do
      super <- shape x y self
      w <- newIORef width
      h <- newIORef height
      returnIO $
           getWidth  .=. readIORef w
       .*. getHeight .=. readIORef h
       .*. setWidth  .=. (\neww -> writeIORef w neww)
       .*. setHeight .=. (\newh -> writeIORef h newh)
       .*. draw      .=. 
           do
              putStr  "Drawing a Rectangle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), width " << self # getWidth <<
                      ls ", height " << self # getHeight <<
                      ls "\n"
       .*. super

-- Square: inherits from Shape
-- Again, it is polymorphic in the types of its fields
-- Like Rectange, in has getWidth and setWidth. But it doesn't
-- have getHeight and setHeight methods. This is a bit contrived,
-- but important for the point we make in Encoding 5 below.

square x y width self
  = do
      super <- shape x y self
      w <- newIORef width
      returnIO $
           getWidth  .=. readIORef w
       .*. setWidth  .=. writeIORef w
       .*. draw      .=. 
           do
              putStr  "Drawing a Square at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), side " << self # getWidth <<
                      ls "\n"
       .*. super


-- Circle: inherits from Shape
-- Again, it is polymorphic in the types of its fields

data GetRadius;    getRadius     = proxy::Proxy GetRadius
data SetRadius;    setRadius     = proxy::Proxy SetRadius

circle x y radius self
  = do
      super <- shape x y self
      r <- newIORef radius
      returnIO $
           getRadius  .=. readIORef r
       .*. setRadius  .=. (\newr -> writeIORef r newr)
       .*. draw       .=. 
           do
              putStr  "Drawing a Circle at:(" <<
                      self # getX << ls "," << self # getY <<
                      ls "), radius " << self # getRadius <<
                      ls "\n"
       .*. super
