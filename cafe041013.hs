{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- This module was sent to the Haskell cafe on 13 Oct 2004.
-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html
-- 3.2 Reference to self


module SelfObj where

import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric2
import Label2

import Data.STRef
import Data.IORef
import Control.Monad.ST
import Control.Monad.Fix

infixr 9 #
m # field = (m .!. field) 

-- A name space for record labels
data MyNS = MyNS
l_get_x   = firstLabel MyNS "get-x"
l_move    = nextLabel l_get_x "move"
l_field_x = nextLabel l_move "field x"
l_print   = nextLabel l_field_x "print"

{-

Ocaml Tutorial:

3.2 Reference to self

A method or an initializer can send messages to self (that is, the
current object). For that, self must be explicitly bound, here to the
variable s (s could be any identifier, even though we will often
choose the name self.

class printable_point x_init =
   object (s)
     val mutable x = x_init
     method get_x = x
     method move d = x <- x + d
     method print = print_int s#get_x
   end;;
let p = new printable_point 7;;
val p : printable_point = <obj>
 
p#print;;
7- : unit = ()

-}

class_printable_point x_init self
  = do
      x <- newIORef x_init
      return $
	   l_field_x .=. x
       .*. l_get_x   .=. readIORef x
       .*. l_move    .=. (\d -> do{v<-readIORef x; writeIORef x (d + v)})
       .*. l_print   .=. ( (self # l_get_x ) >>= print )
       .*. emptyRecord

testp1 = do
	  print "testp1"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  p <- mfix (class_printable_point 7)
	  p # l_get_x >>= print
	  p # l_move $ 2
	  p # l_get_x >>= print
	  p # l_print -- Note, the latter prints the state of the mutated obj!
	  print "OK"

{-

Ocaml Tutorial:

3.7 Inheritance

We illustrate inheritance by defining a class of colored points that
inherits from the class of points. This class has all instance
variables and all methods of class point, plus a new instance variable
c and a new method color.

class colored_point x (c : string) =
   object
     inherit point x
     val c = c
     method color = c
   end;;

let p' = new colored_point 5 "red";;
val p' : colored_point = <obj>
 
p'#get_x, p'#color;;
- : int * string = (5, "red")

-}

-- Inheritance is simple: just adding methods...

l_color  = nextLabel l_print "color"

class_colored_point x_init color self
  = do
      p <- class_printable_point x_init self
      return $ l_color .=. (return color) .*. p

testp2 = do
	  print "testp2"
	  -- Note that 'mfix' plays the role of 'new' in the OCaml code...
	  p  <- mfix (class_printable_point 7)
	  p' <- mfix (class_colored_point 5 "red")
	  do{ x <- p' # l_get_x; c <- p' # l_color; print (x,c) }
	  print "OK"

{- 

Ocaml Tutorial:

3.4 Virtual methods

It is possible to declare a method without actually defining it, using
the keyword virtual. ...

class virtual abstract_point x_init =
   object (self)
     val mutable x = x_init
     method virtual get_x : int
     method get_offset = self#get_x - x_init
     method virtual move : int -> unit
   end;;

class point x_init =
   object
     inherit abstract_point x_init
     method get_x = x
     method move d = x <- x + d
   end;;

-}

l_offset  = nextLabel l_color "offset"

-- Note, compared with printable_point, the we just removed the field l_get_x 
-- That made the class uninstantiatable!
-- No need for any a language extension for virtual, abstract.

class_abstract_printable_point x_init self
  = do
      x <- newIORef x_init
      return $
	   l_field_x .=. x
       .*. l_offset  .=. ((self # l_get_x ) >>= (\v -> return$ v - x_init))
       .*. l_print   .=. ( (self # l_get_x ) >>= print )
       .*. emptyRecord

class_concrete_printable_point x_init self
  = do
      p <- class_abstract_printable_point x_init self -- inherit...
      return $ 
      -- add the missing (pure virtual) methods
           l_get_x   .=. (readIORef (self # l_field_x))
       .*. l_move    .=. (\d -> do{v<-readIORef (self # l_field_x); 
                                      writeIORef (self # l_field_x) (d + v)})
       .*. p

testp3 = do
	  print "testp3"
	  -- Note, if the latter is uncommented; we get the 
          -- desired instantiation error.
	  p  <- mfix (class_concrete_printable_point 7)
	  p # l_get_x >>= print
	  p # l_move $ 2
	  p # l_offset >>= print
	  p # l_get_x >>= print
	  p # l_print
	  print "OK"
