{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{- 

OOHaskell (C) 2004, 2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- Experiments with covariant return types, as in Java 5
-- Thanks are due to Robin Green
-- who triggered this effort and provided the sample scenario.

-}

module CovariantReturn where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)
import TypeCastGeneric2

infixr 9 #
m # field = (m .!. field) 


-- We use the example of 1D points and color points we have seen
-- earlier (see Selfish.hs)
data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;     moveX     = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveX     .=. (\d -> modifyIORef x (+d))
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord

-- and extend it to a colored point
data GetColor; getColor = proxy::Proxy GetColor

-- Inheritance is simple: just adding methods ...
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

data GetP1; getP1               = proxy::Proxy GetP1
data GetP2; getP2               = proxy::Proxy GetP2

-- Note that vector is a polymorphic class! It is equivalent to a C++
-- template 
-- template class Vector<PointT> { PointT p1,p2; ...};
-- In Haskell, we don't need to do anything special for declaring such
-- a polymorphic class

vector (p1::p) (p2::p) self =
   do
      returnIO $
           getP1    .=. returnIO p1
       .*. getP2    .=. returnIO p2
       .*. print    .=. do self # getP1 >>= ( # print )
			   self # getP2 >>= ( # print )
       .*. emptyRecord



norm v =
    do
    p1 <- v # getP1
    p2 <- v # getP2
    x1 <- p1 # getX
    x2 <- p2 # getX
    return (abs (x1 - x2))


test1 = do
	p1  <- mfix (printable_point 0)
	p2  <- mfix (printable_point 5)
	cp1 <- mfix (colored_point 10 "red")
	cp2 <- mfix (colored_point 25 "red")
	v1  <- mfix (vector p1 p2)
	-- Note that cv1 is in depth subtyping to v1!
	cv1 <- mfix (vector cp1 cp2)
	v1 # print
	cv1 # print
	putStrLn "Length of v1"
        norm v1 >>= Prelude.print
	-- Now, pass a cv1 to a function that expects a just a vector
	-- This shows that cv1 is substitutable for a cv
	putStrLn "Length of colored cv1"
        norm cv1 >>= Prelude.print
	putStrLn "OK"

{-   Some old stuff
 
-- Note: the getWidth field could be associated with width itself
-- rather than with IO width. However, the getCrossSection field is definitely
-- associated with an IO action: creation of an object of a class
-- rectangle. Therefore, for uniformity, we associate all fields
-- with IO actions.
class_cuboid width height depth self
  = do
      returnIO $
           getWidth        .=. returnIO width
       .*. getHeight       .=. returnIO height
       .*. getDepth        .=. returnIO depth
       .*. getCrossSection .=. mfix (class_rectangle width height)
       .*. emptyRecord

-- A subtype of a cuboid. 
-- We override the method getCrossSection to have a co-variant return type
-- We have to use 
-- (super .-. getCrossSection) rather than .<. super
class_cube width self
  = do
      super <- class_cuboid width width width self
      returnIO $
           getCrossSection .=. mfix (class_square width)
       .*. (super .-. getCrossSection)

-- compute the volume of a cuboid
handle_cuboid cuboid =
    do
    xs <- cuboid # getCrossSection
    w  <- xs # getWidth
    h  <- xs # getHeight
    d  <- cuboid # getDepth
    return (d * w * h)

test1 = do
	cuboid <- mfix (class_cuboid 10 20 30)
	cube   <- mfix (class_cube 40)
	putStrLn "Volume of cuboid"
        handle_cuboid cuboid >>= print
	-- Now, pass a cube to a function that expects a cuboid
	-- This shows that cube is substitutable for a cuboid
	putStrLn "Volume of cube"
        handle_cuboid cube >>= print
	print "OK"
-}

-- Now, to place vectors and colored vectors into the same homogeneous
-- list, we need deep'narrow rather than simple narrow as before

test2 = do
	p1  <- mfix (printable_point 0)
	p2  <- mfix (printable_point 5)
	cp1 <- mfix (colored_point (10::Int) "red")
	cp2 <- mfix (colored_point 25 "red")
	v1  <- mfix (vector p1 p2)
	-- Note that cv1 is in depth subtyping to v1!
	cv1 <- mfix (vector cp1 cp2)
	let vectors = [deep'narrow v1, deep'narrow cv1]
		      `asTypeOf` [v1]
        -- The following would raise a type error:
	-- with a clear message
	-- let vectors = [v1, cv1]
	-- The following also raises an error, with a message
	-- that essentially says that GetColor method is missing:
	-- Indeed, v1 cannot be coerced to cv1!
	-- let vectors = [deep'narrow v1, deep'narrow cv1]
	--	      `asTypeOf` [cv1]
	putStrLn "Vectors"
        mapM_ (\v -> do
	               v # print
	               putStr "Length is "; norm v >>= Prelude.print)
	      vectors


data ItsRecord
data ItsIO
data ItsOther

class IsIORecord a b | a -> b
instance IsIORecord (Record y) ItsRecord
instance IsIORecord (IO y) ItsIO
instance TypeCast f ItsOther => IsIORecord a f

class DeepNarrow a b where
    deep'narrow :: a -> b

instance (IsIORecord a f, DeepNarrow' f a b) => DeepNarrow a b where
    deep'narrow = deep'narrow' (undefined::f)

class DeepNarrow' f a b where
    deep'narrow' :: f -> a -> b

instance TypeCast a b => DeepNarrow' ItsOther a b where
    deep'narrow' _ = typeCast

instance DeepNarrow' ItsRecord r (Record HNil) where
    deep'narrow' _ _ = emptyRecord

-- Note: all of the following constraints were suggested
-- by GHC. I merely wrote the body of the function deep'narrow' below
-- the compiler suggested to add a few constrainst, and I did so.
instance ( DeepNarrow' ItsRecord (Record r) (Record r')
         , H2ProjectByLabels (HCons l HNil) r (HCons (l, v) HNil) rout
	 , IsIORecord v f, DeepNarrow' f v v'
	 , HRLabelSet (HCons (l,v') r')
	 )
    => DeepNarrow' ItsRecord (Record r) (Record (HCons (l,v') r')) where
    deep'narrow' _ r = result
	where
	r'       = (deep'narrow r) :: (Record r')
	labels   = HCons (undefined::l) HNil
	Record (HCons (l,v) HNil) = hProjectByLabels labels  r
	(v'::v') = deep'narrow v
	result   = (l,v') .*. r'
		  
instance DeepNarrow a b => DeepNarrow' ItsIO (IO a) (IO b) where
    deep'narrow' _ a = a >>= (return . deep'narrow)


main = do
	  test1
	  test2
