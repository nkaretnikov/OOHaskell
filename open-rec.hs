{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}

-- Open recursion
-- In the following, we refer to the tutorial "Objects in Caml"
-- http://caml.inria.fr/ocaml/htmlman/manual005.html
-- 3.2                              Reference to self




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


data MyNS = MyNS -- a name space for record labels
-- There are many different ways to declare labels. Here's one
-- See the simplest implementation in the HObjMap.hs file
-- data Field1 = Field1

l_get_x   = firstLabel MyNS "get-x"
l_move    = nextLabel l_get_x "move"
l_field_x = nextLabel l_move "field x"
l_print  = nextLabel l_field_x "print"

{- Ocaml Tutorial:
3.2                              Reference to self


A method or an initializer can send messages to self (that is, the current
object). For that, self must be explicitly bound, here to the variable s (s
could be any identifier, even though we will often choose the name self.)

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

Dynamically, the variable s is bound at the invocation of a method. In
particular, when the class printable_point is inherited, the variable s will be
correctly bound to the object of the subclass.

-- we shall see that below...
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


{- Ocaml Tutorial:
3.3                                Initializers


Let-bindings within class definitions are evaluated before the object is
constructed. It is also possible to evaluate an expression immediately after
the object has been built. Such code is written as an anonymous hidden method
called an initializer. Therefore, is can access self and the instance
variables.

class printable_point x_init =
   let origin = (x_init / 10) * 10 in
   object (self)
     val mutable x = origin
     method get_x = x
     method move d = x <- x + d
     method print = print_int self#get_x
     initializer print_string "new point at "; self#print; print_newline()
   end;;

-}
-- We can model initializers just like in OCaml: use a dedicated label
-- `initializer. We introduce a function `new' that, after doing mfix,
-- will locate the label `initializer' and run the corresponding action
-- This is all trivial, so we skip this.

{- Ocaml Tutorial:
3.7                                 Inheritance


We illustrate inheritance by defining a class of colored points that inherits
from the class of points. This class has all instance variables and all methods
of class point, plus a new instance variable c and a new method color.
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

A point and a colored point have incompatible types, since a point has no
method color. However, the function get_x below is a generic function applying
method get_x to any object p that has this method (and possibly some others,
which are represented by an ellipsis in the type). Thus, it applies to both
points and colored points.

let get_succ_x p = p#get_x + 1;;
val get_succ_x : < get_x : int; .. > -> int = <fun>

get_succ_x p + get_succ_x p';;
- : int = 8

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
	  let get_succ_x obj = obj # l_get_x >>= (return . (+ 1))
	  get_succ_x p  >>= print
	  get_succ_x p' >>= print
	  print "OK"

{- Ocaml Tutorial:
3.4                               Virtual methods

It is possible to declare a method without actually defining it, using the
keyword virtual. This method will be provided later in subclasses. A class
containing virtual methods must be flagged virtual, and cannot be instantiated
(that is, no object of this class can be created). It still defines type
abbreviations (treating virtual methods as other methods.)

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
-- That made class_abstract_printable_point uninstantiatable!!!
-- No need for any keywords like virtual, abstract, etc...
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
	  -- Note, if the latter is uncommented
	  --     p'   <- mfix (class_abstract_printable_point 7)
	  -- we see an error
	  -- field get_x is missing:
          -- (HFind (Label HZero MyNS) HNil n,
	  --  HLookupByHNat n HNil (IO a))
	  -- The label ought to be printed out better...
	  -- Anyway, it shows that the abstract class cannot be
	  -- instantiated...
	  -- Note, how everything falls into place without any new keywords
	  p  <- mfix (class_concrete_printable_point 7)
	  p # l_get_x >>= print
	  p # l_move $ 2
	  p # l_offset >>= print
	  p # l_get_x >>= print
	  p # l_print -- Note, the latter prints the state of the mutated obj!
	  print "OK"

{- Ocaml Tutorial:
3.5                               Private methods


Private methods are methods that do not appear in object interfaces. They can
only be invoked from other methods of the same object.

class restricted_point x_init =
   object (self)
     val mutable x = x_init
     method get_x = x
     method private move d = x <- x + d
     method bump = self#move 1
   end;;
class restricted_point :
  int ->
  object
    val mutable x : int
    method bump : unit
    method get_x : int
    method private move : int -> unit
  end
 
let p = new restricted_point 0;;
val p : restricted_point = <obj>
 
p#move 10;;
This expression has type restricted_point
It has no method move
 
p#bump;;
- : unit = ()
-}

-- One way to make methods private -- just don't put them into the
-- record
-- In the following class, the field 'x' is private
-- In our formulation, there is no real difference between a value
-- field and a method. 
-- Note, we can easily achieve sharing of the methods bodies accross
-- different objects created by the same class. Just use let bindings,
-- regular Haskell way...
class_printable_point_private_x x_init self
  = do
      x <- newIORef x_init
      return $
           l_get_x   .=. readIORef x
       .*. l_move    .=. (\d -> do{v<-readIORef x; writeIORef x (d + v)})
       .*. l_print   .=. ( (self # l_get_x ) >>= print )
       .*. emptyRecord

-- Whereas inheritance adds methods, restriction of a method to private
-- removes methods...
-- Unlike the OCaml code above, here we remove the method
-- l_get_x from the public interface of the printable class
-- The method can still be used privately (by the l_print method)
-- But it cannot be used publicly
testp4 = do
	  print "testp4"
	  pu  <- mfix (class_printable_point 7)
	  let p = pu .-. l_get_x -- l_get_x becomes private
	  -- p # l_get_x >>= print  -- can no longer be used
	  p # l_print -- this is OK
	  p # l_move $ 2
	  -- p # l_get_x >>= print  -- can no longer be used: type error
	  p # l_print
	  print "OK"


{- Ocaml Tutorial:
3.6                              Class interfaces


Class interfaces are inferred from class definitions. They may also be defined
directly and used to restrict the type of a class. Like class declarations,
they also define a new type abbreviation.
In addition to program documentation, class interfaces can be used to constrain
the type of a class. Both instance variables and concrete private methods can
be hidden by a class type constraint. Public and virtual methods, however,
cannot.

-}

-- Don't have time for that... The idea of restricting to the interface
-- is similar to the removing of a method in testp4 above



{- Ocaml Tutorial:

3.8                            Multiple inheritance


Multiple inheritance is allowed. Only the last definition of a method is kept:
the redefinition in a subclass of a method that was visible in the parent class
overrides the definition in the parent class. Previous definitions of a method
can be reused by binding the related ancestor. Below, super is bound to the
ancestor printable_point. The name super is a pseudo value identifier that can
only be used to invoke a super-class method, as in super#print.

class printable_colored_point y c =
   object (self)
     val c = c
     method color = c
     inherit printable_point y as super
     method print =
       print_string "(";
       super#print;
       print_string ", ";
       print_string (self#color);
       print_string ")"
   end;;
-}

-- Actually here we give a better example: a diamond inheritance
-- and a conversion of the open recursion into the closed recursion
-- We implement the following diagram
{--
       abstract_printable_point     abstract_printable_point
         /                \                  |
     concr_pt1         concr_pt2          concr_pt3
              \	             |               |
               \             |           color_pt
                \            |         /
                 \           |       /
                          final_pt 
--}

-- So, final_pt has *two* instances of abstract_printable_point
-- One of them is shared with concr_pt1  and concr_pt2, and one
-- of them inherited from color_pt.
-- We can't do that in C++!

-- the following will be shared across all point objects....
move_method self = l_move .=. (\d -> do{v<-readIORef (self # l_field_x); 
					writeIORef (self # l_field_x) (d + v)})

class_concr_pt1 x_init self
  = do
      p <- class_abstract_printable_point x_init self -- inherit...
      return $ 
      -- add the missing (pure virtual) methods
           l_get_x   .=. (readIORef (self # l_field_x))
       .*. (move_method self)
       .*. p

class_concr_pt2 x_init self
  = do
      p <- class_abstract_printable_point x_init self -- inherit...
      return $ 
      -- add the missing (pure virtual) methods
           l_get_x   .=. ((return 42):: IO Int)
       .*. (move_method self)
       .*. p

class_concr_pt3 x_init self
  = do
      p <- class_abstract_printable_point x_init self -- inherit...
      return $ 
      -- add the missing (pure virtual) methods
           l_get_x   .=. (readIORef (self # l_field_x))
       .*. (move_method self)
       .*. p

-- Here, we override the l_print method in the parent class
class_c3 x_init color self
  = do
      p <- class_concr_pt3 x_init self
      return $ 
           l_color .=. ((return color)::IO String)
       .*. l_print .=. do{ v <- (self # l_get_x);
			   putStr "class_concr_pt3: ";
			   putStr color; putStr " ";
			   print v}
       .*. (p .-. l_print)


-- compose the final class
class_pt_final x_init color self 
  = do
     super1 <- class_concr_pt1 x_init self
     super2 <- class_concr_pt2 x_init self -- share the same self!
     super3 <- mfix (class_c3 x_init color) -- do not share self!
     let new_print =
	     do
	       putStr "super1: "; (super1 # l_print)
	       putStr "super2: "; (super2 # l_print)
	       putStr "super3: "; (super3 # l_print)
     -- composing the final object. We should use record union 
     -- here. But I'm lazy and so I do the composition by hand...
     return $ 
	      l_print .=. new_print

	  .*. l_move  .=. (\d -> do{ super1 # l_move $ d; super2 # l_move $ d;
				     super3 # l_move $ d})

	  .*. (hAppend
	       (((((super3 .-. l_print) .-. l_move) .-. l_get_x)
		.-. l_offset) .-. l_field_x)
	       (hAppend
		(((((super2 .-. l_print) .-. l_move) .-. l_get_x)
		  .-. l_offset) .-. l_field_x)
		(((super1 .-. l_print) .-. l_move))))


testp5 = do
	  print "testp5"
	  p  <- mfix (class_pt_final 7 "blue")
	  p # l_print -- this is OK
	  p # l_move $ 2
	  p # l_print -- note that super3 isn't really shared
	  print "OK"

-- Note, try
-- :type class_pt_final
-- It shows like 50 type variables. Very impressive...

{- Ocaml Tutorial:
3.9                            Parameterized classes


Reference cells can be implemented as objects. The naive definition fails to
typecheck:

class ref x_init =
   object 
     val mutable x = x_init
     method get = x
     method set y = x <- y
   end;;
Some type variables are unbound in this type:
  class ref :
    'a ->
    object val mutable x : 'a method get : 'a method set : 'a -> unit end
The method get has type 'a where 'a is unbound

The reason is that at least one of the methods has a polymorphic type (here,
the type of the value stored in the reference cell), thus either the class
should be parametric, or the method type should be constrained to a monomorphic
type. A monomorphic instance of the class could be defined by:

class ref (x_init:int) =
   object
     val mutable x = x_init
     method get = x
     method set y = x <- y
   end;;
class ref :
  int ->
  object val mutable x : int method get : int method set : int -> unit end

A class for polymorphic references must explicitly list the type parameters in
its declaration. Class type parameters are always listed between [ and ]. The
type parameters must also be bound somewhere in the class body by a type
constraint.

class ['a] ref x_init =
   object
     val mutable x = (x_init : 'a)
     method get = x
     method set y = x <- y
   end;;
class ['a] ref :
  'a -> object val mutable x : 'a method get : 'a method set : 'a -> unit end
 
let r = new ref 1 in r#set 2; (r#get);;
- : int = 2
-}

-- That is not the problem in our case
-- If we do
-- :t class_printable_point
-- we see that our class already polymorphic:
-- Num a => a

