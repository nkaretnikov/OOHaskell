{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

-- Link HList source directory to HList subdir.
-- Use gmake simple
-- which expands to ghci -i./HList  simple.hs

-- Simple objects, classes, interfaces (not involving open recursion,
-- i.e., the use of self)
-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html
-- Sec 3.1 Classes and objects




module SimpleObj where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4
import Data.Typeable -- needed for showing labels
import Data.STRef
import Data.IORef
import Control.Monad.ST

{-

------------------------------------------------------------------------
-- Warm up: Simulating open products (records as in OCaml)
-- In OCaml:
--let foo f = f#fld1;;
--val foo : < fld1 : 'a; .. > -> 'a = <fun>

-}

infixr 9 #
m # field = m .!. field

data Field1 deriving Typeable; field1 = proxy::Proxy Field1

foo f = f # field1

testfoo  = foo (field1 .=. True .*. emptyRecord)



------------------------------------------------------------------------
-- OCaml Objects tutorial
-- Sec 3.1 Classes and objects

{- Ocaml Tutorial:

 #class point =
    object
      val mutable x = 0
      method get_x = x
      method move d = x <- x + d
    end;;
 class point :
   object val mutable x : int method get_x : int method move : int ->unit end

-}

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveD;    moveD    = proxy::Proxy MoveD

-- Note, here the field 'x' here is intentionally public -- just as in the
-- Ocaml code above

point = 
   do
      x <- newIORef 0
      return
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveD    .=.(\d -> do modifyIORef x ((+) d))
       .*. emptyRecord


{- Ocaml Tutorial:

 #let p = new point;;
 val p : point = <obj>
 Note that the type of p is point. This is an abbreviation
 automatically defined by the class definition above. It stands for the
 object type <get_x : int; move : int -> unit>, listing the methods of
 class point along with their types.
 We now invoke some methods to p:

 #p#get_x;;
 - : int = 0

 #p#move 3;;
 - : unit = ()

 #p#get_x;;
 - : int = 3

-}

-- Note how the code below mimics the Ocaml code above.
-- The only notable difference is the use of the monad, needed for mutables.
-- Note: no `new' necessary. But see the case with open rec.

myFirstOOP =
  do
     p <- point
     p # getX >>= print
     p # moveD $ 3
     p # getX >>= print

-- The field mutableX is public and can be manipulated directly.

mySecondOOP =
  do 
     p <- point
     writeIORef (p # mutableX) 42
     p # getX >>= print

    
{-

{- Ocaml Tutorial:

The evaluation of the body of a class only takes place at object creation time.
Therefore, in the following example, the instance variable x is initialized to
different values for two different objects.

let x0 = ref 0;;
val x0 : int ref = {contents = 0}
 
class point =
   object
     val mutable x = incr x0; !x0
     method get_x = x
     method move d = x <- x + d
   end;;
class point :
  object val mutable x : int method get_x : int method move : int -> unit end
 
new point#get_x;;
- : int = 1
 
new point#get_x;;
- : int = 2
-}


testp2 = do
	  print "testp2"
	  x0 <- newIORef 0
	  -- Note, we declare a _local_ object
	  -- we also re-use the labels that we declared early
	  let point = 
		  do
		  readIORef x0 >>= (writeIORef x0 . (+1))
		  x <- readIORef x0 >>= newIORef
		  return $ 
	                      l_field_x .=. x
	                  .*. l_get_x   .=. readIORef x
			  .*. l_move    .=.(\d -> do{v<-readIORef x; writeIORef x (d + v)})
			  .*. emptyRecord
	  point >>= ( # l_get_x) >>= print
	  point >>= ( # l_get_x) >>= print

{- Ocaml Tutorial:
The class point can also be abstracted over the initial values of the x
coordinate.
The parameter x_init is, of course, visible in the whole body of the
definition, including methods. For instance, the method get_offset in the class
below returns the position of the object relative to its initial position.

class point x_init =
   object
     val mutable x = x_init
     method get_x = x
     method get_offset = x - x_init
     method move d = x <- x + d
   end;;
-}

l_offset = nextLabel l_field_x "offset"

class_point x_init
  = do
      x <- newIORef x_init
      return $ 
	          l_field_x .=. x
	      .*. l_get_x   .=. readIORef x
              .*. l_offset  .=. do{v<-readIORef x; return$ v - x_init}
              .*. l_move    .=.(\d -> do{v<-readIORef x; writeIORef x (d + v)})
              .*. emptyRecord


testp3 = do
	  print "testp3"
	  p <- class_point 1
	  p # l_get_x >>= print
	  p # l_move $ 2
	  p # l_get_x >>= print
	  p # l_offset >>= print
	  print "OK"

{- Ocaml Tutorial:

Expressions can be evaluated and bound before defining the object body of the
class. This is useful to enforce invariants. For instance, points can be
automatically adjusted to the nearest point on a grid, as follows:

class adjusted_point x_init =
   let origin = (x_init / 10) * 10 in
   object
     val mutable x = origin
     method get_x = x
     method get_offset = x - origin
     method move d = x <- x + d
   end;;

This ability provides class constructors as can be found in other languages.
Several constructors can be defined this way to build objects of the same class
but with different initialization patterns; an alternative is to use
initializers, as decribed below in section 3.3.

-}

class_adj_point x_init
  = do
      let origin = (x_init `div` 10) * 10
      x <- newIORef origin
      return $ 
	          l_field_x .=. x
	      .*. l_get_x   .=. readIORef x
              .*. l_offset  .=. do{v<-readIORef x; return$ v - origin}
              .*. l_move    .=.(\d -> do{v<-readIORef x; writeIORef x (d + v)})
              .*. emptyRecord


testp4 = do
	  print "testp4"
	  p <- class_adj_point 11
	  p # l_get_x >>= print
	  p # l_move $ 2
	  p # l_get_x >>= print
	  p # l_offset >>= print
	  print "OK"

{-
 The ST stuff needs more work, due to the fact 's' in ST s is delibreately
 polymorphic. We need a shallow comparision... I know how to do that,
 but let's leave it for later...

class_pointST x_init
  = do
      x <- newSTRef x_init
      return $ 
	           l_get_x  .=. readSTRef x
               .*. l_offset .=. do{v<-readSTRef x; return$ v - x_init}
               .*. l_move   .=.(\d -> do{v<-readSTRef x; writeSTRef x (d + v)})
               .*. emptyRecord

printST buf val = 
    do
      v <- readSTRef buf
      writeSTRef buf (v ++ (show val) ++ "\n")


testoST1 = 
    let 
      test :: ST s String
      test = do
	      printbuf <- newSTRef ""
	      p <- class_pointST (1::Int)
--	      (p # l_get_x) >>= (printST printbuf)
	      (p # l_move $ (2::Int))::ST s ()
	      readSTRef printbuf
    in runST test
-}

-}
