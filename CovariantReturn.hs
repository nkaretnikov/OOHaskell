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
import TypeCastGeneric2

infixr 9 #
m # field = (m .!. field) 

data GetWidth; getWidth               = proxy::Proxy GetWidth
data GetHeight; getHeight             = proxy::Proxy GetHeight
data GetDepth; getDepth               = proxy::Proxy GetDepth
data GetSide; getSide                 = proxy::Proxy GetSide
data GetCrossSection; getCrossSection = proxy::Proxy GetCrossSection

class_rectangle width height self
  = do
      returnIO $
           getWidth    .=. returnIO width
       .*. getHeight   .=. returnIO height
       .*. emptyRecord

class_square width self
  = do
      super <- class_rectangle width width self
      returnIO $
           -- I know this is silly when we already have getWidth, but 
           -- please humour me
           -- I needed to add SOME method to make the type of class_square 
           -- different to the
           -- type of class_rectangle, for the purposes of this demonstration
           getSide     .=. returnIO width
       .*. super

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

-- Now, test narrowing a cube to a cuboid
-- Not only cube is substitutable to a cuboid: a cube
-- can be safely coerced to a cuboid

test2 = do
	(cuboid::cuboid) <- mfix (class_cuboid (10::Int) (20::Int) (30::Int))
	cube   <- mfix (class_cube (40::Int))
	let --cuboids:: [cuboid]
            cuboids = [cuboid, deep'narrow cube]
            -- The following would raise a type error:
            -- there is no way a cuboid can be narrowed to a cube!
            --cuboids = [cube, deep'narrow cuboid]
	putStrLn "Volumes of cuboids"
        mapM_ (\cb -> handle_cuboid cb >>= print) cuboids
	print "OK"



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
	 , HZip ls vs r
	 , HFind l ls n
	 , HLookupByHNat n vs v
	 , HLookupByHNat n ls l
	 , IsIORecord v f, DeepNarrow' f v v'
	 , HExtend (l,v') (Record r') (Record (HCons (l,v') r'))

         , HZip tx ty r'
	 , HExtend l tx (HCons l tx)
	 , HExtend v' ty (HCons v' ty)
	 , HMember l tx HFalse
	 , HLabelSet tx
	 )
    => DeepNarrow' ItsRecord (Record r) (Record (HCons (l,v') r')) where
    deep'narrow' _ r@(Record ur) = result
	where
	r'       = (deep'narrow r) :: (Record r')
	(ls,vs)  = hUnzip ur
	n        = hFind (undefined::l) ls
	v        = hLookupByHNat n vs
	l        = hLookupByHNat n ls
	(v'::v') = deep'narrow v
	result   = hExtend (l,v') r'
		  
instance DeepNarrow a b => DeepNarrow' ItsIO (IO a) (IO b) where
    deep'narrow' _ a = a >>= (return . deep'narrow)


main = do
	  test1
	  test2
