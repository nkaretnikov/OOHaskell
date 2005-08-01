{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

Illustration of a circular buffer, which is a naturally polymorphic
collection with strongly-typed access. We also illustrate classes in
local scope.

-}

module CircBuffer where

import OOHaskell
import Data.Array.IO

data EmptyP;       emptyP     = proxy::Proxy EmptyP
data Insert;       insert     = proxy::Proxy Insert
data Delete;       delete     = proxy::Proxy Delete
data Underflow;    underflow  = proxy::Proxy Underflow
data Overflow;     overflow   = proxy::Proxy Overflow

{-

-- Note, the interface is polymorphic in the type of the value, a
type PPInterface a
 = Record (  (Proxy GetX    , IO a)
         :*: (Proxy MoveTo  , a -> IO ())
         :*: (Proxy Print    , IO ())
         :*: HNil )

-}


-- The textbook `Stack' example, a bit spiced up
-- we illustrate: parameterized objects (aka classes/constructors), 
-- effects during the construction, using self. 

class_circ_buffer capacity self
  = do
      readP  <- newIORef 0
      writeP <- newIORef 0
      buffer <- newArray_ (0,capacity) :: IO (IOArray Int a)
      let capacity1 = capacity + 1 -- one cell as a guard
      returnIO $
	   emptyP .=. liftM2 (==) (readIORef readP) (readIORef writeP)
       .*. insert   .=. (\e -> do 
			         self # overflow
			         wp <- readIORef writeP
			         writeArray buffer wp e
			         writeIORef writeP (wp + 1 `mod` capacity1))
       .*. delete   .=. do  
			    self # underflow 
			    rp <- readIORef readP
			    e <- readArray buffer rp
			    writeIORef readP (rp + 1 `mod` capacity1)
			    return e
-- perhaps we should remove the overflow/underflow: makes example
-- a bit too complex
       .*. overflow   .=. do
 			     rp <- readIORef readP
 			     wp <- readIORef writeP
 			     let wp1 = wp + 1 `mod` capacity1
 			     if wp1 == rp then error "Overflow" else return ()
       .*. underflow  .=. do 
			     f <- self # emptyP 
			     if f then error "Underflow" else return ()
       .*. emptyRecord

testb1 buffer_class = do
	  print "testb1"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  b <- mfix (buffer_class 7)
	  (b # insert) 'a'
	  (b # insert) 'b'
	  (b # delete) >>= print
	  print "OK"

testb2 buffer_class = do
	  print "testb2"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  b <- mfix (buffer_class 7)
	  (b # insert) (1::Int)
	  -- The following will cause a type error
	  -- (b # insert) 'a'
	  (b # delete) >>= print
	  print "OK"

test1 = do
	  print "test1"
	  testb1 class_circ_buffer
	  testb2 class_circ_buffer
	  print "OK"

{-

And here we come across something interesting. Our Buffer is a
polymorphic collection. Elements can be of any type. Until
very recently, one can't express this in Java and in C#. And here 
we didn't have to do anything at all. No extra
declarations necessary. The compiler has figured it all out (try
uncommenting the statement in testb2). We get the fully
static type checking of all collection operations. Another point: no
declaration for the type of testset: the compiler figured it all
out. We can print it out (and explain the printout).

-}


test2 =
    let class_logging_buffer capacity self = 
	    do
	     cb <- class_circ_buffer capacity self
	     let superInsert = cb .!. insert
	     returnIO $
		      insert .=. (\e -> do
				         putStr "Inserting: "
				         print e
				         superInsert e)
		      .*. (cb .-. insert)
    in do
         print "test2"
         testb1 class_logging_buffer
         testb2 class_logging_buffer
	 print "OK"

{-

Here we wrote a function that takes a constructor (a class!) and 
superimposed on the insert function. We
define a new class in a lexical scope. We pass the extended class to
the old function testb1, which would call the overridden function.
In C++ one can't define a new class
in a local scope. Classes are top level objects. In OOHaskell, an OO
class is just a parameterized object, which is first-class. We can
declare a class in a local scope -- without giving this any thought.

-}


main = do test1; test2
