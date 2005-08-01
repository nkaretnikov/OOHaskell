{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004--2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- A method that returns Self

-- We cannot return just polymorphic Self,
-- but we can return Self that is cast to the approporiate interface.
-- This is the same situation as in C++/Java
-- (where the return type of a method must be declared anyway).

-}


module SelfReturn where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


-- All the labels that are needed

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;    moveX    = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print
data Me;       me       = proxy::Proxy Me


-- Note, the interface is polymorphic in the type of the value, a

type PPInterface a
   = Record (  GetX  :=: IO a
           :*: MoveX :=: (a -> IO ())
           :*: Print :=: IO ()
           :*: HNil )


-- The base class for this experiment

printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO
        $  mutableX  .=. x
       .*. getX      .=. readIORef x
       .*. moveX     .=. (\d -> modifyIORef x (+d))
       .*. print     .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord


-- A subclass that returns self

self_returning_point (x_init::a) self =
   do
      super <- printable_point x_init self
      returnIO
          -- Returning self directly is caught by occurs check!
          -- $  me .=. self
          $  me .=. (narrow self :: PPInterface a)
         .*. super

testp1 = do
	  Prelude.print "testp1"
	  p <- mfix (self_returning_point 7)
	  p # getX >>= Prelude.print
	  p # moveX $ 2
	  p # getX >>= Prelude.print
	  p # print -- Note, the latter prints the state of the mutated obj!
	  Prelude.print "OK"


-- Another label

data Color;  colorP   = proxy::Proxy Color


-- Variation on self_returning_point

self_returning_point' x_init color self
  = do
      p <- self_returning_point x_init self
      let super_print = p .!. print
      return $ 
	     colorP .=. (returnIO color) 
	 .*. print  .=. do Prelude.print "Color point"
                           Prelude.print color
                           super_print
	 .*. (p .-. print)


-- More test cases

testp2 = do
	  Prelude.print "testp2"
	  p <- mfix (self_returning_point' 5 "red")
	  p # print
	  Prelude.print "OK"

testp3 = do
	  Prelude.print "testp3"
	  pp <- mfix (self_returning_point 7)
	  pc <- mfix (self_returning_point' 5 "red")
	  pp # print
	  pc # print
	  let pp1 = (pc # me)
	  -- Note that although pp is of a type Un-colored point,
	  -- it prints that it is a colored point nevertheless
	  pp1 # print
	  Prelude.print "OK"


main = do testp1; testp2; testp3
