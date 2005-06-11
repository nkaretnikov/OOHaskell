{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- The classic Shapes example
-- http://www.angelfire.com/tx4/cus/shapes/
-- We shall try to emulate the `classical' C++ implementation
-- http://www.angelfire.com/tx4/cus/shapes/cpp.html
-- Please compare it with the Haskell solution in the shapes suite.
-- http://www.angelfire.com/tx4/cus/shapes/haskell.html

-}


module Shapes where

import OOHaskell

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

shape (x_init::t) (y_init::t) self
  = do
      x <- newIORef x_init
      y <- newIORef y_init
      returnIO $
           getX     .=. readIORef x
       .*. getY     .=. readIORef y
       .*. setX     .=. (\(newx::t) -> writeIORef x newx)
       .*. setY     .=. (\(newy::t) -> writeIORef y newy)
       .*. moveTo   .=. (\(newx::t) (newy::t) -> do
                                        (self # setX) newx
                                        (self # setY) newy 
                                        returnIO () -- optional
                        )
       .*. rMoveTo  .=. (\(deltax::t) (deltay::t) ->
                 do
                    x  <- self # getX
                    y  <- self # getY
                    () <- (self # moveTo) (x + deltax) (y + deltay)
                    returnIO () -- optional
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


-- This interface is used in Encoding 1 below.  When we want to make a
-- list of abstract Shapes, then we cast objects to this interface.

type Shape a
 = Record (  (Proxy GetX    , IO a)
         :*: (Proxy GetY    , IO a)
         :*: (Proxy SetX    , a -> IO ())
         :*: (Proxy SetY    , a -> IO ())
         :*: (Proxy MoveTo  , a -> a -> IO ())
         :*: (Proxy RMoveTo , a -> a -> IO ())
         :*: (Proxy Draw    , IO ())
         :*: HNil )


-- In fact this interface would be sufficent
type Shape' a
 = Record (  (Proxy RMoveTo , a -> a -> IO ())
         :*: (Proxy Draw    , IO ())
         :*: HNil )



{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 1
-- We build a homogeneous list of shapes.
-- We use coercion to an interface to harmonise the different shapes.

myShapesOOP =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble :: [Shape Int]
           scribble = [narrow s1, narrow s2]
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}

--
-- Experimental code; don't mind.
-- Should go elsewhere eventually.
-- 

class UpCastTo f t1 t2 | t1 -> t2
 where
  upCastTo :: f -> t1 -> t2

instance UpCastTo f ty1 ty2 => UpCastTo f (tx -> ty1) ty2
 where
  upCastTo f _ = upCastTo f (undefined::ty1) 

instance Narrow f t => UpCastTo (Record f) (Record t) (Record t)
 where
  upCastTo f _ = narrow f


class LubNarrow a b c | a b -> c
 where
  lubNarrow :: a -> b -> (c,c)

instance ( HZip la va a
         , HZip lb vb b
         , HTIntersect la lb lc
         , H2ProjectByLabels lc a c aout
         , H2ProjectByLabels lc b c bout
         , HRLabelSet c
         )
      => LubNarrow (Record a) (Record b) (Record c)
 where
  lubNarrow ra@(Record a) rb@(Record b) =
     ( hProjectByLabels lc ra
     , hProjectByLabels lc rb
     )
   where
    lc = hTIntersect la lb
    (la,_) = hUnzip a
    (lb,_) = hUnzip b

class LubCast l e | l -> e
 where
  lubCast :: l -> [e]

instance ( LubNarrow h h' e
         )
      => LubCast (HCons h (HCons h' HNil)) e
 where
  lubCast (HCons h (HCons h' _)) = [fst ee, snd ee]
   where
    ee = lubNarrow h h'

instance ( LubCast (HCons h (HCons h'' t)) e'
         , LubCast (HCons h' (HCons h'' t)) e''
         , LubNarrow e' e'' e
         , LubCast (HCons e (HCons h'' t)) e
         )
      => LubCast (HCons h (HCons h' (HCons h'' t))) e
 where
  lubCast (HCons h (HCons h' t)) = fst e : ( snd e : tail r )
   where
    e' = lubCast (HCons h t)
    e'' = lubCast (HCons h' t)
    e = lubNarrow (head e') (head e'')
    r = lubCast (HCons (fst e) t)

yaShapesOOP =
  do
       -- set up array of shapes
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = lubCast (HCons s1 (HCons s2 HNil))
       
       -- iterate through the array
       -- and handle shapes polymorphically
       mapM_ (\shape -> do
                           shape # draw
                           (shape # rMoveTo) 100 100
                           shape # draw)
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
--       arec # setRadius $ 40
       arec # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 2
-- We build a heterogeneous list of shapes.
-- We use a heterogeneous map over the list.
-- The polymorphic function of the map is an Apply instance.
-- We place the label constraints in this instance.

testHList =
  do
       -- set up list of shapes.
       s1 <- mfix (rectangle (10::Int) (20::Int) 5 6)
       s2 <- mfix (circle (15::Int) 25 8)
       let scribble = s1 `HCons` (s2 `HCons` HNil)
       
       -- iterate through the list
       -- and handle shapes polymorphically
       hMapM_ (undefined::FunOnShape) scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) 15 15)
       arec # setWidth $ 30
       arec # draw


-- A type code for the polymorphic function on shapes
data FunOnShape -- a type code only!

-- The polymorphic function on shapes
instance ( HasField (Proxy Draw) r (IO ())
         , HasField (Proxy RMoveTo) r (Int -> Int -> IO ())
         )
      => Apply FunOnShape r (IO ())
  where
    apply _ x = do
                   x # draw
                   (x # rMoveTo) 100 100
                   x # draw


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


-- Encoding 3
-- This time we combine the use of existential quantification
-- with OOHaskell objects. That is we have quantify the methods
-- that are going to be invoked for the shape objects.
-- This quantification requires constraints which are not first-class.

testExist =
  do
       -- set up list of shapes.
       s1 <- mfix (rectangle (10::Int) (20::Int) (5::Int) (6::Int))
       s2 <- mfix (circle (15::Int) (25::Int) (8::Int))
       let scribble = [ WrapShape s1
                      , WrapShape s2 ]
       
       -- iterate through the list
       -- and handle shapes polymorphically
       mapM_ ( \(WrapShape shape) -> do
                  shape # draw
                  (shape # rMoveTo) 100 100
                  shape # draw )
             scribble

       -- call a rectangle specific function
       arec <- mfix (rectangle (0::Int) (0::Int) (15::Int) (15::Int))
       arec # setWidth $ 30
       arec # draw


-- The well-quantified existential wrapper
data WrapShape =
 forall x. ( HasField (Proxy Draw) x (IO ())
           , HasField (Proxy RMoveTo) x (Int -> Int -> IO ())
           ) => WrapShape x


{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}
{-----------------------------------------------------------------------------}


main = do 
          putStrLn "testCoerce"; myShapesOOP
          putStrLn "testLub"; yaShapesOOP
          putStrLn "testHList";  testHList
          putStrLn "testExist";  testExist

-- :t mfix $ rectangle (1::Int) (2::Int) (3::Int) (4::Int)
