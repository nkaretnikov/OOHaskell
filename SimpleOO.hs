{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This set of samples deals with simple objects, classes, interfaces
(not yet involving open recursion, i.e., the use of self). In the
following we quote from the tutorial "Objects in Caml", Sec 3.1
Classes and objects, http://caml.inria.fr/ocaml/htmlman/manual005.html
The idea is to reconstruct these examples very directly in OOHaskell.

NOTE on overlapping: We need overlapping instances SOLELY for the sake
of Label4 below. We could use (and have used) other ways of
representing labels, such as Label2. The latter requires no
overlapping instances. However, Label4 labels look better in types.

See the Makefile for running this file.

-}


module SimpleOO where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4
import Data.Typeable -- needed for showing labels
import Data.IORef
import GHC.IOBase


{-

Warm up: Simulating open products (records as in OCaml)
In OCaml:

let foo f = f#fld1;;
val foo : < fld1 : 'a; .. > -> 'a = <fun>

-}


-- We reuse record selection from HList
infixr 9 #
m # field = m .!. field

-- This is how we define labels
data Field1 deriving Typeable; field1 = proxy::Proxy Field1

-- This is how record selection looks like.
foo f = f # field1

-- So this is how we actually demo record access.
testfoo  = foo (field1 .=. True .*. emptyRecord)



------------------------------------------------------------------------
-- OCaml Objects tutorial
-- Sec 3.1 Classes and objects

{- Ocaml Tutorial:

 #class point =
    object
      val mutable x = 0
      method getX = x
      method move d = x <- x + d
    end;;
 class point :
   object val mutable x : int method getX : int method move : int ->unit end

-}

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX deriving Typeable; mutableX = proxy::Proxy MutableX
data GetX  deriving Typeable;    getX     = proxy::Proxy GetX
data Move  deriving Typeable;    move     = proxy::Proxy Move

-- Note, here the field 'x' here is intentionally public -- just as in the
-- Ocaml code above

point = 
   do
      x <- newIORef 0
      returnIO
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. move     .=. (\d -> modifyIORef x ((+) d))
       .*. emptyRecord



{- Ocaml Tutorial:

 #let p = new point;;
 val p : point = <obj>

 Note that the type of p is point. This is an abbreviation
 automatically defined by the class definition above. It stands for the
 object type <getX : int; move : int -> unit>, listing the methods of
 class point along with their types.
 We now invoke some methods to p:

 #p#getX;;
 - : int = 0

 #p#move 3;;
 - : unit = ()

 #p#getX;;
 - : int = 3

-}

-- Note how the code below mimics the Ocaml code above.
-- The only notable difference is the use of the monad, needed for mutables.
-- Note: no `new' necessary. But see the case with open rec.

myFirstOOP =
  do
     p <- point -- no need for new!
     p # getX >>= print
     p # move $ 3
     p # getX >>= print

-- The field mutableX is public and can be manipulated directly.

mySecondOOP =
  do 
     p <- point
     writeIORef (p # mutableX) 42
     p # getX >>= print

    

{- Ocaml Tutorial:

The evaluation of the body of a class only takes place at object creation time.
Therefore, in the following example, the instance variable x is initialized to
different values for two different objects.

let x0 = ref 0;;
val x0 : int ref = {contents = 0}
 
class incrementing_point =
   object
     val mutable x = incr x0; !x0
     method getX  = x
     method move d = x <- x + d
   end;;
class incrementing_point :
  object val mutable x : int method getX : int method move : int -> unit end
 
new incrementing_point#getX;;
- : int = 1
 
new incrementing_point#getX;;
- : int = 2
-}


-- We define a point class that introduces a local reference to be
-- increment through all object creations. We also re-use the labels
-- that we declared earlier.

incrementing_point = 
   do 
      x0 <- newIORef 0
      returnIO (
        do modifyIORef x0 (+1)
           x <- readIORef x0 >>= newIORef
           returnIO
             $  mutableX .=. x
            .*. getX     .=. readIORef x
            .*. move     .=. (\d -> modifyIORef x ((+) d))
            .*. emptyRecord
       )

testBody =
   do
      localPoint <- incrementing_point
      localPoint >>= ( # getX ) >>= print
      localPoint >>= ( # getX ) >>= print



{- Ocaml Tutorial:

The class point can also be abstracted over the initial values of the
x coordinate.  The parameter x_init is, of course, visible in the
whole body of the definition, including methods. For instance, the
method getOffset in the class below returns the position of the
object relative to its initial position.

class para_point x_init =
   object
     val mutable x    = x_init
     method getX      = x
     method getOffset = x - x_init
     method move d    = x <- x + d
   end;;

-}


-- A new label is needed for this example.
data GetOffset; getOffset = proxy::Proxy GetOffset


-- OCaml's parameterised class is nothing but a function.
para_point x_init
   = do
        x <- newIORef x_init
        returnIO
          $  mutableX  .=. x
         .*. getX      .=. readIORef x
         .*. getOffset .=. queryIORef x (\v -> v - x_init)
         .*. move      .=. (\d -> modifyIORef x ((+) d))
         .*. emptyRecord


-- A shortcut for IORef processing. Is that somewhere in the libraries?
queryIORef ref f = readIORef ref >>= returnIO . f

testPara =
   do
      p <- para_point 1
      p # getX >>= print
      p # move $ 2
      p # getX >>= print
      p # getOffset >>= print



{- Ocaml Tutorial:

Expressions can be evaluated and bound before defining the object body of the
class. This is useful to enforce invariants. For instance, points can be
automatically adjusted to the nearest point on a grid, as follows:

class adjusted_point x_init =
   let origin = (x_init / 10) * 10 in
   object
     val mutable x    = origin
     method getX      = x
     method getOffset = x - origin
     method move d    = x <- x + d
   end;;

This ability provides class constructors as can be found in other languages.
Several constructors can be defined this way to build objects of the same class
but with different initialization patterns; an alternative is to use
initializers, as decribed below in section 3.3.

-}

adjusted_point x_init
   = do
        let origin = (x_init `div` 10) * 10
        x <- newIORef origin
        returnIO $  mutableX  .=. x
                .*. getX      .=. readIORef x
                .*. getOffset .=. queryIORef x (\v -> v - origin)
                .*. move      .=. (\d -> modifyIORef x ((+) d))
                .*. emptyRecord

testConstr =
   do
      p <- adjusted_point 11
      p # getX >>= print
      p # move $ 2
      p # getX >>= print
      p # getOffset >>= print


main = do 
          putStrLn "myFirstOOP"  ; myFirstOOP
          putStrLn "mySecondOOP" ; mySecondOOP
          putStrLn "testBody"    ; testBody
          putStrLn "testPara"    ; testPara
          putStrLn "testConstr"  ; testConstr
