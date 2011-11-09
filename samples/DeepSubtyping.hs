{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fcontext-stack=100 #-}

{- 

OOHaskell (C) 2004--2010, Oleg Kiselyov, Ralf Laemmel

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
      return
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX    .=. (\d -> modifyIORef x (+d))
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


{-

-- Note that vector is a polymorphic class!
-- It is equivalent to a C++ template class:
--   class Vector<PointT> { PointT p1,p2; ...};
-- In OOHaskell, we don't need to do declare such polymorphism.

-}

-- Polymorphic vectors

vector p1 p2 self =
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


-- Illustration of polymorphism in deep subtypes

test1 = do
           putStrLn "test1"
           p1   <- mfix (printable_point 0)
           p2   <- mfix (printable_point 5)
           cp1  <- mfix (colored_point 10 "red")
           cp2  <- mfix (colored_point 25 "red")
           v    <- mfix (vector p1 p2)
           cv   <- mfix (vector cp1 cp2)
           -- Note that cv is in depth subtyping to v.
           putStrLn "Vector:"
           v  # print
           putStrLn "Colored vector:"
           cv # print
           putStrLn "Length of vector:"
           norm v  >>= Prelude.print
           -- Now, pass a cv to a function that expects just a vector.
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


-- Some extra test cases for DeepNarrow

data Label1; label1 = proxy::Proxy Label1
data Label2; label2 = proxy::Proxy Label2
data Label3; label3 = proxy::Proxy Label3
data Label4; label4 = proxy::Proxy Label4

-- o2 extends o1 (width subtyping)
o1 = returnIO emptyRecord
o2 = do super <- o1; returnIO $ label1 .=. returnIO (1::Int) .*. super

-- o3 takes objects of types o o'
o3 o o' = returnIO
          $
             label2 .=. o
         .*. label3 .=. (\(x::Int) -> o)
         .*. label4 .=. (\(x::Int) y -> const o (y `asTypeOf` o'))
         .*. emptyRecord

-- o5 is a deep subtype of o4
o4 = o3 o1 o2
o5 = o3 o2 o1

o6 = o3 o1 o1
o7 = o3 o2 o2

-- We test the deep'narrow operation
l1 = [deep'narrow o5,o4] 
l2 = [deep'narrow o6,o4] 

--l1bad = [deep'narrow o7,o6] 
--l2bad = [deep'narrow o4,o6] 


main = do test1; test2
