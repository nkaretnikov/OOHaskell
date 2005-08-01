{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

OOHaskell (C) 2004, 2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

Straightforward experiments with covariant return types, as in Java 5.
We note that covariant return types are type-safe and conceptually not
very challenging. (The situation is all different to covariant
argument types, which are dealt with in CovariantArgs.hs.) Thanks are
due to Robin Green who suggested the sample scenario.

-}

module CovariantReturn where

import OOHaskell


-- Various labels

data GetWidth; getWidth               = proxy::Proxy GetWidth
data GetHeight; getHeight             = proxy::Proxy GetHeight
data GetDepth; getDepth               = proxy::Proxy GetDepth
data GetSide; getSide                 = proxy::Proxy GetSide
data GetCrossSection; getCrossSection = proxy::Proxy GetCrossSection


-- A base class for rectangles

rectangle width height self
  = do
      returnIO $
           getWidth    .=. returnIO width
       .*. getHeight   .=. returnIO height
       .*. emptyRecord


-- A subtype of rectangle, square.
-- Note that we do not sublass for code reuse here.
-- We rather refine the superclass into its subclass.

square width self
  = do
      super <- rectangle width width self
      returnIO $
           -- Of course we could as well omit this getter.
           -- (The side length is identical to getWidth and getHeight.)
           -- (Due to the extension, square is a proper subtype of rectangle).
           getSide .=. returnIO width
       .*. super


-- A 3D base class

cuboid width height depth self
  = do
      returnIO
        $  getWidth        .=. returnIO width
       .*. getHeight       .=. returnIO height
       .*. getDepth        .=. returnIO depth
       .*. getCrossSection .=. mfix (rectangle width height)
       .*. emptyRecord


-- A subtype of a cuboid. 
-- We override the method getCrossSection to have a covariant return type.
-- We have to use (super .-. getCrossSection) rather than .<. super

cube width self
  = do
      super <- cuboid width width width self
      returnIO
         $  getCrossSection .=. mfix (square width)
        .*. (super .-. getCrossSection)


-- Compute the volume of a cuboid

volume aCuboid
  = do
       xs <- aCuboid # getCrossSection
       w  <- xs # getWidth
       h  <- xs # getHeight
       d  <- aCuboid # getDepth
       return (d * w * h)


-- Time for a test case.

test1 = do
           print "test1"
           aCuboid <- mfix (cuboid 10 20 30)
           aCube   <- mfix (cube 40)
           putStrLn "Volume of cuboid"
           volume aCuboid >>= print
           -- Now, pass a cube to a function that expects a cuboid.
           -- This shows that cube is substitutable for a cuboid.
           putStrLn "Volume of cube"
           volume aCube >>= print
           print "OK"


-- Now, test narrowing a cube to a cuboid.
-- Not only cube is substitutable to a cuboid,
-- a cube can be safely coerced to a cuboid.

test2 = do
           print "test2"
           aCuboid <- mfix (cuboid (10::Int) (20::Int) (30::Int))
           aCube   <- mfix (cube (40::Int))
           let cuboids = [aCuboid, deep'narrow aCube]
           -- The following would raise a type error.
           -- There is no way a cuboid can be narrowed to a cube!
           -- cuboids = [aCube, deep'narrow aCuboid]
           putStrLn "Volumes of cuboids"
           mapM_ (\cb -> volume cb >>= print) cuboids
           print "OK"


main = do
	  test1
	  test2
