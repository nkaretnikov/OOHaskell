{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

-- A method that returns Self

-- We cannot return just polymorphic Self
-- But we can return Self that is cast to the approporiate interface
-- This is the same situation as in C++/Java
-- (where the return type of a method must be declared anyway)

-}

module SelfReturn where

import OOHaskell

infixr 9 #
m # field = (m .!. field) 


data FieldX;   fieldX   = proxy::Proxy FieldX
data GetX;     getX     = proxy::Proxy GetX
data MoveTo;   moveTo   = proxy::Proxy MoveTo
data Print;    printIt  = proxy::Proxy Print
data ID;       myself   = proxy::Proxy ID

-- Note, the interface is polymorphic in the type of the value, a
type PPInterface a
 = Record (  (Proxy GetX    , IO a)
         :*: (Proxy MoveTo  , a -> IO ())
         :*: (Proxy Print    , IO ())
         :*: HNil )

class_printable_point (x_init::a) self
  = do
      x <- newIORef x_init
      returnIO $
	   fieldX .=. x
       .*. getX   .=. readIORef x
       .*. moveTo    .=. (\d -> modifyIORef x (+ d))
       .*. printIt   .=. ( (self # getX ) >>= print )
       .*. myself    .=. (narrow self :: PPInterface a)
       .*. emptyRecord

testp1 = do
	  print "testp1"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  p <- mfix (class_printable_point 7)
	  p # getX >>= print
	  p # moveTo $ 2
	  p # getX >>= print
	  p # printIt -- Note, the latter prints the state of the mutated obj!
	  print "OK"



data Color;  colorP   = proxy::Proxy Color

class_colored_point x_init color self
  = do
      p <- class_printable_point x_init self
      let super_print = p .!. printIt
      return $ 
	     colorP .=. (returnIO color) 
	 .*. printIt .=. do {print "Color point"; print color; super_print}
	 .*. (p .-. printIt)

testp2 = do
	  print "testp2"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  p <- mfix (class_colored_point 5 "red")
	  p # printIt
--	  do{ x <- p # getX; c <- p # colorP; print (x,c) }
	  print "OK"

testp3 = do
	  print "testp3"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  pp <- mfix (class_printable_point 7)
	  pc <- mfix (class_colored_point 5 "red")
	  pp # printIt
	  pc # printIt
	  let pp1 = (pc # myself)
	  -- Note that although pp is of a type Un-colored point,
	  -- it prints that it is a colored point nevertheless
	  pp1 # printIt
	  print "OK"


main = do testp1; testp2; testp3
