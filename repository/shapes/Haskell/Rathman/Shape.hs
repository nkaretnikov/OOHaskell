{-# OPTIONS -fglasgow-exts #-}

module Shape(Shape, ExistentialShape(MakeExistentialShape), getX, getY, setX, setY, moveTo, rMoveTo, draw)

   where

   -- declare method interfaces for the shape superclass
   class Shape a where
      getX :: a -> Int
      getY :: a -> Int
      setX :: a -> Int -> a
      setY :: a -> Int -> a
      moveTo :: a -> Int -> Int -> a
      rMoveTo :: a -> Int -> Int -> a
      draw :: a -> IO()

   -- declare the constructor for the existential type
   data ExistentialShape =
      forall a. Shape a => MakeExistentialShape a

   -- map the methods for the existential type
   instance Shape ExistentialShape where
     getX (MakeExistentialShape a) = getX a
     getY (MakeExistentialShape a) = getY a
     setX (MakeExistentialShape a) newx = MakeExistentialShape(setX a newx)
     setY (MakeExistentialShape a) newy = MakeExistentialShape(setY a newy)
     moveTo (MakeExistentialShape a) newx newy = MakeExistentialShape(moveTo a newx newy)
     rMoveTo (MakeExistentialShape a) deltax deltay = MakeExistentialShape(rMoveTo a deltax deltay)
     draw (MakeExistentialShape a) = draw a
