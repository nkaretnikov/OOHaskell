{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

OOHaskell (C) 2004, 2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

We illustrate a non-trivial substitution scenario where vectors over
plain points vs. colored points are passed to the same function. We
face a scenario of deep subtyping since the types of vectors differ in
the point type and thereby in the result types of getters that are
provided by all vectors. Polymorphic functions on vectors immediately
apply to colored vectors as well. When we need to homogenise the types
of vectors of different point types, then a deep variation on the
ubiquitous narrow operation is needed.

-}


module DeepSubtyping where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


-- We use the example of 1D points and color points
-- that we have seen earlier (see Selfish.hs)

data MutableX;  mutableX = proxy::Proxy MutableX
data GetX;      getX     = proxy::Proxy GetX
data MoveX;     moveX    = proxy::Proxy MoveX
data Print;     print    = proxy::Proxy Print
data GetColor;  getColor = proxy::Proxy GetColor

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX     .=. (\d -> modifyIORef x (+d))
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord

colored_point x_init (color::String) self =
   do
        super <- printable_point x_init self
        return 
            $  getColor .=. (returnIO color)
	   .*.  (print .=. (
                  do  putStr "Point at - "; super # print
                      putStr "color  - "; Prelude.print color )
                 .<. super)


-- Now we define a vector, specified by two points

data GetP1; getP1 = proxy::Proxy GetP1
data GetP2; getP2 = proxy::Proxy GetP2


-- Note that vector is a polymorphic class!
-- It is equivalent to a C++ template class:
--   class Vector<PointT> { PointT p1,p2; ...};
-- In OOHaskell, we don't need to do declare such polymorphism.

vector (p1::p) (p2::p) self =
   do 
      p1r <- newIORef p1
      p2r <- newIORef p2
      returnIO
        $  getP1    .=. readIORef p1r
       .*. getP2    .=. readIORef p2r
       .*. print    .=. do self # getP1 >>= ( # print )
			   self # getP2 >>= ( # print )
       .*. emptyRecord


-- A polymorphic operation on vectors

norm v =
   do
      p1 <- v  # getP1; p2 <- v  # getP2
      x1 <- p1 # getX;  x2 <- p2 # getX
      return (abs (x1 - x2))


-- We illustrate polymorphism in deep subtypes.

test1 = do
           putStrLn "test1"
           p1  <- mfix (printable_point 0)
           p2  <- mfix (printable_point 5)
           cp1 <- mfix (colored_point 10 "red")
           cp2 <- mfix (colored_point 25 "red")
           v  <- mfix (vector p1 p2)
           -- Note that cv is in depth subtyping to v.
           cv <- mfix (vector cp1 cp2)
           putStrLn "Vector:"
           v  # print
           putStrLn "Colored vector:"
           cv # print
           putStrLn "Length of vector:"
           norm v >>= Prelude.print
           -- Now, pass a cv to a function that expects a just a vector.
           -- This shows that cv is substitutable for v.
           putStrLn "Length of colored vector:"
           norm cv >>= Prelude.print
           putStrLn "OK"


-- Now, to place vectors and colored vectors into the same homogeneous
-- list, we need deep'narrow rather than simple narrow as before.

test2 = do
           putStrLn "test2"
           p1  <- mfix (printable_point 0)
           p2  <- mfix (printable_point 5)
           cp1 <- mfix (colored_point (10::Int) "red")
           cp2 <- mfix (colored_point 25 "red")
           v   <- mfix (vector p1 p2)
           cv  <- mfix (vector cp1 cp2)
           --
           -- The following would raise a type error with a clear message.
           --
           -- let vectors = [v, cv]
           -- 
           -- The following narrows the colored vector. This is Ok.
           --
           let vectors = [v, deep'narrow cv]
           --
           -- The following also raises an error, with a message
           -- that essentially says that GetColor method is missing:
           -- Indeed, v cannot be coerced to cv!
           --
           -- let vectors = [deep'narrow v, cv]
           --
           putStrLn "Vectors"
           mapM_ (\v -> do
                           v # print
                           putStr "Length is "; norm v >>= Prelude.print)
                 vectors
           putStrLn "OK"


main = do test1; test2
