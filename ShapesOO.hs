{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- We need overlapping instances SOLELY for the sake of Label4 below.
-- We could use (and have used) other ways of representing labels,
-- such as Label2. The latter requires no overlapping instances.
-- However, Label4 labels look better in types.

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html
-- Please compare it with the traditional Haskell stuff
-- http://www.angelfire.com/tx4/cus/shapes/haskell.html

module ShapesOO where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast, typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import qualified TypeCastGeneric2
import Label4

import Data.IORef
import Control.Monad.Fix
import GHC.IOBase

infixr 9 #
m # field = (m .!. field) 

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

class_shape x_init y_init self
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
                    x <- self # getX
                    y <- self # getY
                    (self # moveTo) (x + deltax) (y + deltay)
                        )
       .*. emptyRecord



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

class_rectangle x y width height self
  = do
      super <- class_shape x y self
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

-- Circle: inherits from Shape
-- Again, it is polymorphic in the types of its fields

data GetRadius;    getRadius     = proxy::Proxy GetRadius
data SetRadius;    setRadius     = proxy::Proxy SetRadius

class_circle x y radius self
  = do
      super <- class_shape x y self
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


-- We don't need to define the following. However, we later want
-- to make a list of abstract Shapes, so we define the interface to
-- cast objects to

type ShapeInterface a
 = Record (  (Proxy GetX    , IO a)
	 :*: (Proxy GetY    , IO a)
	 :*: (Proxy SetX    , a -> IO ())
	 :*: (Proxy SetY    , a -> IO ())
	 :*: (Proxy MoveTo  , a -> a -> IO ())
	 :*: (Proxy RMoveTo , a -> a -> IO ())
	 :*: (Proxy Draw    , IO ())
	 :*: HNil )


main = do
       -- set up array to the shapes.
       s1 <- mfix (class_rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (class_circle (15::Int) (25::Int) (8::Int))
       let scribble :: [ShapeInterface Int]
           scribble = [upCast s1, upCast s2]
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (class_rectangle (0::Int) (0::Int) (15::Int) (15::Int))
       arec # setWidth $ 30
       arec # draw
