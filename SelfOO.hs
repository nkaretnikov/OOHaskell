{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module goes beyond the simple examples in SimpleOO.hs. The new
samples here start to use self - i.e., open recursion. A number of key
OO concepts are demonstrated including basics like inheritance. In the
following we again quote from the tutorial "Objects in Caml".

NOTE on overlapping: See SimpleOO.hs. In addition, we use overlapping
for some advanced features of GhcRecord to experiment with a secondary
model of virtual methods.

See the Makefile for running this file.

-}


module SelfOO where


import CommonMain hiding (HDeleteMany, hDeleteMany, TypeCast,typeCast)
import GhcSyntax
import GhcRecord
import GhcExperiments
import TypeEqBoolGeneric
import TypeEqGeneric1
import TypeCastGeneric1
import Label4

import Data.STRef
import Data.IORef
import Control.Monad.ST
import Control.Monad.Fix
import GHC.IOBase

infixr 9 #
m # field = (m .!. field) 

-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveD;    moveD    = proxy::Proxy MoveD
data OOPrint;  ooprint  = proxy::Proxy OOPrint


{- Ocaml Tutorial: 3.2 Reference to self

A method or an initializer can send messages to self (that is, the
current object). For that, self must be explicitly bound, here to the
variable s (s could be any identifier, even though we will often
choose the name self.)

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
particular, when the class printable_point is inherited, the variable
s will be correctly bound to the object of the subclass.

-- We shall see that below ...

-}


printable_point x_init s =
   do
      x <- newIORef x_init
      returnIO
        $  mutableX .=. x
       .*. getX     .=. readIORef x
       .*. moveD    .=. (\d -> modifyIORef x ((+) d))
       .*. ooprint  .=. ((s # getX ) >>= print)
       .*. emptyRecord

mySelfishOOP
   = do
        -- Note that 'mfix' plays the role of 'new' in the OCaml code...
        p <- mfix (printable_point 7)
        p # getX >>= print
        p # moveD $ 2
        p # getX >>= print
        -- Note, we print the state of the mutated obj!
        p # ooprint



{- Ocaml Tutorial: 3.3 Initializers

Let-bindings within class definitions are evaluated before the object
is constructed. It is also possible to evaluate an expression
immediately after the object has been built. Such code is written as
an anonymous hidden method called an initializer. Therefore, is can
access self and the instance variables.

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


{- Ocaml Tutorial: 3.7 Inheritance

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

-}

-- We need another label.
data GetColor; getColor = proxy::Proxy GetColor

-- Inheritance is simple: just adding methods ...
colored_point x_init (color::String) self
   = do
        p <- printable_point x_init self
        returnIO $ getColor .=. (returnIO color) .*. p

testInheritance
   = do
        p' <- mfix (colored_point 5 "red")
        x  <- p' # getX
        c  <- p' # getColor
        print (x,c)


{- Ocaml Tutorial: 3.7 Inheritance

A point and a colored point have incompatible types, since a point has no
method color. However, the function get_x below is a generic function applying
method get_x to any object p that has this method (and possibly some others,
which are represented by an ellipsis in the type). Thus, it applies to both
points and colored points.

let get_succ_x p = p#get_x + 1;;
val get_succ_x : < get_x : int; .. > -> int = <fun>

get_succ_x p + get_succ_x p';;
- : int = 8

-- BTW, why 8?
-- The points in the scope suggest 14!

-}

testGeneric
   = do
        p  <- mfix (printable_point 7)
        p' <- mfix (colored_point 5 "red")
        let get_succ_x obj = obj # getX >>= (returnIO . (+ 1))
        x  <- get_succ_x p
        x' <- get_succ_x p'
        print $ x+x'



{- Ocaml Tutorial: 3.4 Virtual methods

It is possible to declare a method without actually defining it, using
the keyword virtual. This method will be provided later in
subclasses. A class containing virtual methods must be flagged
virtual, and cannot be instantiated (that is, no object of this class
can be created). It still defines type abbreviations (treating virtual
methods as other methods.)

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

-- A new label is needed for this example.
data GetOffset; getOffset = proxy::Proxy GetOffset

-- Note, compared with printable_point, we omitted the virtual methods.
-- That made abstract_point uninstantiatable!!!

abstract_point x_init self
  = do
      x <- newIORef x_init
      returnIO $
	   mutableX  .=. x
       .*. getOffset .=. ((self # getX ) >>= (\v -> returnIO $ v - x_init))
       .*. ooprint   .=. ((self # getX ) >>= print )
       .*. emptyRecord

concrete_point x_init self
   = do
        p <- abstract_point x_init self -- inherit ...
        returnIO
        -- add the missing (pure virtual) methods
          $  getX  .=. readIORef (self # mutableX)
         .*. moveD .=. (\d -> modifyIORef (self # mutableX) ((+) d))
         .*. p

testVirtual
   = do
        p  <- mfix (concrete_point 7)
        --
        -- Note, if the latter is uncommented
        --     p' <- mfix (abstract_point 7)
        -- we see an error that means "field getX missing"
        -- which reads as follows:
        --  No instances for ( HFind (Proxy GetX) HNil n,
        --                     HLookupByHNat n HNil (IO a) )
        --
        p # getX >>= print
        p # moveD $ 2
        p # getOffset >>= print
        p # getX >>= print
        p # ooprint


-- This abstract point class mentions the type of the virtual methods.

abstract_point' x_init self
  = do
      x <- newIORef x_init
      returnIO $
	   mutableX  .=. x
       .*. getX      .=. (proxy::Proxy (IO Int))
       .*. moveD     .=. (proxy::Proxy (Int -> IO ()))
       .*. getOffset .=. ((self # getX ) >>= (\v -> returnIO $ v - x_init))
       .*. ooprint   .=. ((self # getX ) >>= print )
       .*. emptyRecord


-- Another label for testing purposes
data MyLabel; myLabel = proxy::Proxy MyLabel


-- This concrete class implements all virtual methods
concrete_point' x_init self
   = do
        p <- abstract_point' x_init self -- inherit ...
        returnIO
        -- use disciplined record update
           $  getX    .=. readIORef (self # mutableX)
          .^. moveD   .=. (\d -> modifyIORef (self # mutableX) ((+) d))
          .^. myLabel .=. ()                -- This line could be activated.
--        .^. myLabel .=. (proxy::Proxy ()) -- A proxy that disables mnew.
          .*. p

-- We introduce a constrained new method to refuse proxy fields in records.
mnew (f::a -> m a) = mfix f
 where
  () = hasNoProxies (undefined::a) 

testVirtual'
   = do
        p <- mnew (concrete_point' 7)
        p # getX >>= print
        p # moveD $ 2
	p # getOffset >>= print
        p # getX >>= print
        p # ooprint



{- Ocaml Tutorial: 3.5 Private methods

Private methods are methods that do not appear in object
interfaces. They can only be invoked from other methods of the same
object.

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

-- We need another label.
data BumpX; bumpX = proxy::Proxy BumpX


-- Private methods are modelled by let bindings.
-- So they are not put into the record of an object.
-- We could achieve sharing of methods between different objects via lets.

restricted_point x_init self
  = do
      x <- newIORef x_init
      let moveD = (\d -> modifyIORef x ((+) d))
      returnIO $
	   mutableX .=. x
       .*. getX     .=. readIORef x
       .*. bumpX    .=. moveD 2
       .*. emptyRecord

testRestricted
   =  do
       p <- mfix (restricted_point 7)
       p # getX >>= print
       p # bumpX
       p # getX >>= print


-- Unlike the OCaml code, we can also remove a method from the interface.
-- This allows us to make methods private for existing objects.
-- We first add the bump method that uses the private method move.

bumping_point x_init self
   = do
        p <- printable_point x_init self
        returnIO
          $  bumpX .=. (self # moveD $ 2)
         .*. p

testRestricted'
   = do
        p  <- mfix (bumping_point 7)
        let p' = p .-. moveD
        p' # ooprint
        p' # bumpX
        p' # ooprint
        -- Attempting access to moveD would result in a type error.



{- Ocaml Tutorial: 3.6 Class interfaces

Class interfaces are inferred from class definitions. They may also be
defined directly and used to restrict the type of a class. Like class
declarations, they also define a new type abbreviation.  In addition
to program documentation, class interfaces can be used to constrain
the type of a class. Both instance variables and concrete private
methods can be hidden by a class type constraint. Public and virtual
methods, however, cannot.

-}

-- Any kind of method can be hidden using the removal approach given above.
-- We could also use a tagging approach (as for virtuals above) to record
-- the status of a method or field to be private. Haskell's existing
-- scoping rules for let bindings etc. seem to be sufficient however.



{- Ocaml Tutorial: 3.8 Multiple inheritance

Multiple inheritance is allowed. Only the last definition of a method
is kept: the redefinition in a subclass of a method that was visible
in the parent class overrides the definition in the parent
class. Previous definitions of a method can be reused by binding the
related ancestor. Below, super is bound to the ancestor
printable_point. The name super is a pseudo value identifier that can
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
-- We implement the following diagram:

{--

                     abstract_point
                 /           |         \
                /            |          \
               /             |           \
              /              |            \            
             /               |             \
     concr_pt1         concr_pt2          concr_pt3
              \	             |               |
               \             |           color_pt3
                \            |         /
                 \           |        /
                          final_pt 

--}



-- The following method will be shared across all point objects.
move_method self
   = moveD .=. (\d -> modifyIORef (self # mutableX) ((+) d))


-- The concrete classes derived from the abstract point class.
concr_pt1 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # mutableX)
         .*. move_method self
         .*. p

concr_pt2 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. ((return 42):: IO Int)
         .*. move_method self
         .*. p

concr_pt3 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # mutableX)
         .*. move_method self
         .*. p


-- We override the print method in the parent class
-- We also access the overriden method akin to the OCaml super.

color_pt3 x_init color self
   = do
        p <- concr_pt3 x_init self
        return
          $  ooprint  .=. ( do 
                               putStrLn "color_pt3:"
                               putStr   " uncolored part - "
                               p # ooprint
                               putStr   "          color - " 
                               print color
                          )
         .<. getColor .=. ((return color)::IO String)
         .*. p


testOverride 
   = do
        p  <- mfix (color_pt3 5 "red")
        p  # ooprint


-- We compose a class which involves multiple inheritance.
-- An object of this class has *two* instances of abstract_point.
-- One of them is shared with concr_pt1  and concr_pt2, and another
-- one is inherited from color_pt. Try this with C++!

pt_final x_init color self 
  = do
     super1 <- concr_pt1 x_init self
     super2 <- concr_pt2 x_init self          -- share the same self!
     super3 <- mfix (color_pt3 x_init color)  -- do not share self!
     let myprint = do
	              putStr "super1: "; (super1 # ooprint)
                      putStr "super2: "; (super2 # ooprint)
                      putStr "super3: "; (super3 # ooprint)
     let mymove  = ( \d -> do
                              super1 # moveD $ d
                              super2 # moveD $ d
	                      super3 # moveD $ d )
     return 
       $    ooprint .=. myprint
      .*.   moveD   .=. mymove
      .*.   emptyRecord
      .<++. super1
      .<++. super2
      .<++. super3


testDiamond
   = do 
        p <- mfix (pt_final 42 "blue")
        p # ooprint    -- all points still agree!
        p # moveD $ 2
        p # ooprint    -- Note that super1,2 are shared, but not 3!

-- Note, try
-- :type pt_final
-- The number of type variables is very impressive!



{- Ocaml Tutorial: 3.9 Parameterized classes

Reference cells can be implemented as objects.
The naive definition fails to typecheck:

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

The reason is that at least one of the methods has a polymorphic type
(here, the type of the value stored in the reference cell), thus
either the class should be parametric, or the method type should be
constrained to a monomorphic type. A monomorphic instance of the class
could be defined by:

class ref (x_init:int) =
   object
     val mutable x = x_init
     method get = x
     method set y = x <- y
   end;;

class ref :
  int ->
  object val mutable x : int method get : int method set : int -> unit end

A class for polymorphic references must explicitly list the type
parameters in its declaration. Class type parameters are always listed
between [ and ]. The type parameters must also be bound somewhere in
the class body by a type constraint.

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

-- That is not the problem in OOHaskell
-- If we do
-- :t printable_point
-- we see that our class already polymorphic:
-- (..., Num a, ...) =>  a -> ...


main = do 
          putStrLn "mySelfishOOP"     ; mySelfishOOP
          putStrLn "testInheritance"  ; testInheritance
          putStrLn "testGeneric"      ; testGeneric
          putStrLn "testVirtual"      ; testVirtual
          putStrLn "testVirtual'"     ; testVirtual'
          putStrLn "testRestricted"   ; testRestricted
          putStrLn "testRestricted'"  ; testRestricted'
          putStrLn "testOverride"     ; testOverride
          putStrLn "testDiamond"      ; testDiamond
