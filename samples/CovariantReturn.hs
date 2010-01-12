{-# LANGUAGE EmptyDataDecls #-}

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
data CrossSection; crossSection = proxy::Proxy CrossSection


-- A base class for rectangles

rectangle width height self
  =  getWidth    .=. width
 .*. getHeight   .=. height
 .*. emptyRecord


-- We refine the rectangles into squares.

square side self
  = let super = rectangle side side self
    in getSide .=. side .*. super


-- We extend rectangles into cuboids.

cuboid width height depth self
  = let super = rectangle width height self
    in
           getDepth     .=. depth
       .*. crossSection .=. fix (rectangle width height)
       .*. super


-- We refine cuboid into cubes. 

cube side self
  = let super = cuboid side side side self
    in     getSide .=. side
       .*. crossSection .=. fix (square side)
       .@. super


{-

In cube, the method crossSection has a covariant result type. Not that
we have to use (super .-. crossSection) rather than .<. super

-}


-- Compute the volume of a cuboid

volume aCuboid
  = let
     xs = aCuboid # crossSection
     w  = xs # getWidth
     h  = xs # getHeight
     d  = aCuboid # getDepth
    in d * w * h


-- Time for a test case.

test1 = do
           print "test1"
           let aCuboid = fix (cuboid 10 20 30)
           let aCube = fix (cube 40)
           putStrLn "Volume of cuboid"
           print $ volume aCuboid
           -- Now, pass a cube to a function that expects a cuboid.
           -- This shows that cube is substitutable for a cuboid.
           putStrLn "Volume of cube"
           print $ volume aCube
           print "OK"


-- Now, test narrowing a cube to a cuboid.
-- Not only cube is substitutable to a cuboid,
-- a cube can be safely coerced to a cuboid.

test2 = do
           print "test2"
           let aCuboid = fix (cuboid (10::Int) (20::Int) (30::Int))
           let aCube   = fix (cube (40::Int))
           -- let cuboids = [aCuboid, aCube] -- Type error!
           let cuboids = [aCuboid, deep'narrow aCube]
           -- The following would raise a type error.
           -- There is no way a cuboid can be narrowed to a cube!
           -- cuboids = [deep'narrow aCuboid, aCube] -- Type error, too!
           putStrLn "Volumes of cuboids"
           mapM_ (\cb -> print $ volume cb) cuboids
           print "OK"


main = do
	  test1
	  test2
