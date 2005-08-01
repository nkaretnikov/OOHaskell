{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module illustrates the notion of self, i.e., open recursion. A
number of key OO concepts are demonstrated including basics like
inheritance. In the following we again quote from the tutorial
"Objects in Caml".

-}


module Selfish where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data MoveX;     moveX     = proxy::Proxy MoveX
data Print;    print    = proxy::Proxy Print


{- Ocaml Tutorial: 3.2 Reference to self

A method or an initializer can send messages to self (that is, the
current object). For that, self must be explicitly bound, here to the
variable s (s could be any identifier, even though we will often
choose the name self.)

class printable_point x_init =
   object (s)
     val mutable x = x_init
     method getX = x
     method moveX d = x <- x + d
     method print = print_int s#getX
   end;;

let p = new printable_point 7;;
val p : printable_point = <obj>

p#moveX 2;;
- : unit = ()

p#print;;
- : unit = ()

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
       .*. moveX     .=. (\d -> modifyIORef x (+d))
--
-- To be revealed later
--     .*. print    .=. print_getX s
--
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord

-- We can share this print_getX method across all the objects
-- that have at least the method getX of type (Show a ) => IO a
-- The objects in question do not have to belong to the same hierarchy.
-- We re-use this function in abstract_point' below.

print_getX self = ((self # getX ) >>= Prelude.print)

-- Note that 'mfix' plays the role of 'new' in the OCaml code...
mySelfishOOP =
   do
      p <- mfix (printable_point 7)
      p # moveX $ 2
      p # print


concrete_printable_point x_init 
  = concrete $ printable_point x_init


-- We test the polymorphism of printable_point
myPolyOOP =
   do
      p  <- mfix (printable_point (1::Int))
      p' <- mfix (printable_point (1::Double))
      p  # moveX $ 2
      p' # moveX $ 2.5
      p  # print
      p' # print

-- We test the first-class status of classes
myFirstClassOOP point_class =
   do
      p <- mfix (point_class 7)
      p # moveX $ 35
      p # print

-- We notice something that was not available in Ocaml. In Ocaml's example,
-- x_init was of the type 'int' -- because operation (+) in Ocaml can operate
-- on integer only. Our point is in contrast, polymorphic. Here's an example
-- to illustrate it:

testPointInt point_class =
    do
      p <- mfix (point_class 7)
      p # moveX $ (2::Int)
      -- Uncomment the following to see the type error. We do statically
      -- track the type of items in our collection.
      -- p # moveX $ (2.0::Double)
      p # print

-- Note something else: our class is first-class.

testPointDouble point_class =
    do
      p <- mfix (point_class 11.0)
      p # moveX $ 3.0
      p # print

testPolyPoints = 
    do
    testPointInt printable_point
    testPointDouble printable_point


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
     method getX = x
     method moveX d = x <- x + d
     method print = print_int self#getX
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
     method get_c = c
   end;;

let p' = new colored_point 5 "red";;
val p' : colored_point = <obj>
 
p'#getX, p'#get_c;;
- : int * string = (5, "red")

-}

-- We need another label.
data GetColor; getColor = proxy::Proxy GetColor

-- Inheritance is simple: just adding methods ...
colored_point x_init (color::String) self =
   do
        super <- printable_point x_init self
        returnIO
            $  getColor .=. (returnIO color)
           .*. super


myColoredOOP =
   do
      p' <- mfix (colored_point 5 "red")
      x  <- p' # getX
      c  <- p' # getColor
      Prelude.print (x,c)


-- We derive a better class of colored points, which prints more accurately.
-- To this end, we access the overriden method akin to the OCaml super.

colored_point' x_init (color::String) self =
   do
      super <- colored_point x_init color self
      return $  print .=. (
                  do  putStr "so far - "; super # print
                      putStr "color  - "; Prelude.print color )
            .<. super

myOverridingOOP =
   do
      p  <- mfix (colored_point' 5 "red")
      p  # print



myOverridingOOP1 = testPointDouble (\d -> colored_point' d "red")


{- Ocaml Tutorial: 3.7 Inheritance

A point and a colored point have incompatible types, since a point has no
method color. However, the function getX below is a generic function applying
method getX to any object p that has this method (and possibly some others,
which are represented by an ellipsis in the type). Thus, it applies to both
points and colored points.

let get_succ_x p = p#getX + 1;;
val get_succ_x : < getX : int; .. > -> int = <fun>

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
        Prelude.print $ x+x'



{- Ocaml Tutorial: 3.4 Virtual methods

It is possible to declare a method without actually defining it, using
the keyword virtual. This method will be provided later in
subclasses. A class containing virtual methods must be flagged
virtual, and cannot be instantiated (that is, no object of this class
can be created). It still defines type abbreviations (treating virtual
methods as other methods.)

-- We have modified this example in a non-essential way:
-- getOffset was removed.
-- print was added.
-- Less code.

class virtual abstract_point x_init =
   object (self)
     val mutable varX = x_init
     method print = print_int self#getX
     method virtual getX : int
     method virtual moveX : int -> unit
   end;;

class concrete_point x_init =
   object
     inherit abstract_point x_init
     method getX = varX
     method moveX d = x <- x + d
   end;;

-}

-- Note, compared with printable_point, we omitted the virtual methods.
-- That made abstract_point uninstantiatable!!!

-- This is an optional part in case we want to fix types of virtuals.

abstract_point (x_init::a) self 
  | const False (
      (narrow self) :: Record (  GetX  :=: IO a
                             :*: MoveX :=: (a -> IO ())
                             :*: HNil ) )
  = undefined

abstract_point (x_init::a) self =
   do
      xRef <- newIORef x_init
      returnIO $
           mutableX  .=. xRef
       .*. print     .=. (self # getX >>= Prelude.print )
       .*. emptyRecord

 -- This is an optional part in case we want to fix types of virtuals.
 where
  _ = narrow self :: Record (  GetX  :=: IO a
                           :*: MoveX :=: (a -> IO ())
                           :*: HNil )


concrete_point x_init self
   = do
        p <- abstract_point x_init self -- inherit ...
        returnIO
        -- add the missing (pure virtual) methods
          $  getX  .=. readIORef (self # mutableX)
         .*. moveX .=. (\d -> modifyIORef (self # mutableX) (+d))
         .*. p

testVirtual
   = do
        p  <- mfix (concrete_point 7)
        --
        -- Note, if the latter is uncommented
        -- p' <- mfix (abstract_point 7)
        -- we see an error that means "field getX missing"
        -- which reads as follows:
        -- (HasField (Proxy GetX) HNil (IO a))
        p # getX >>= Prelude.print
        p # moveX $ 2
        p # getX >>= Prelude.print
        p # print


-- This abstract point class mentions the type of the virtual methods.

abstract_point' x_init self
  = do
      x <- newIORef x_init
      returnIO $
	   mutableX  .=. x
       .*. getX      .=. (proxy::Proxy (IO Int))
       .*. moveX      .=. (proxy::Proxy (Int -> IO ()))
       .*. print     .=. print_getX self -- now we reuse this function
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
          .^. moveX   .=. (\d -> modifyIORef (self # mutableX) (+d))
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
        p # getX >>= Prelude.print
        p # moveX $ 2
        p # getX >>= Prelude.print
        p # print



{- Ocaml Tutorial: 3.5 Private methods

Private methods are methods that do not appear in object
interfaces. They can only be invoked from other methods of the same
object.

class restricted_point x_init =
   object (self)
     val mutable x = x_init
     method getX = x
     method private moveX d = x <- x + d
     method bump = self#moveX 1
   end;;

class restricted_point :
  int ->
  object
    val mutable x : int
    method bump : unit
    method getX : int
    method private moveX : int -> unit
  end
 
let p = new restricted_point 0;;
val p : restricted_point = <obj>
 
p#moveX 10;;
This expression has type restricted_point
It has no method moveX
 
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
      let moveX = (\d -> modifyIORef x (+d))
      returnIO $
	   mutableX .=. x
       .*. getX     .=. readIORef x
       .*. bumpX    .=. moveX 2
       .*. emptyRecord

testRestricted
   =  do
       p <- mfix (restricted_point 7)
       p # getX >>= Prelude.print
       p # bumpX
       p # getX >>= Prelude.print


-- Unlike the OCaml code, we can also remove a method from the interface.
-- This allows us to make methods private for existing objects.
-- We first add the bump method that uses the private method moveX.

bumping_point x_init self
   = do
        p <- printable_point x_init self
        returnIO
          $  bumpX .=. (self # moveX $ 2)
         .*. p

testRestricted'
   = do
        p  <- mfix (bumping_point 7)
        let p' = p .-. moveX
        p' # print
        p' # bumpX
        p' # print
        -- Attempting access to moveX would result in a type error.



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
-- the status of a method or field to be private.



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
     concrete_point1   concrete_point2  concrete_point3
              \	             |             /
               \             |            /
                \            |           /
                 \           |          /
                        heavy_point 

--}



-- The following method will be shared across all point objects.
move_method self
   = moveX .=. (\d -> modifyIORef (self # mutableX) (+d))


-- The concrete classes derived from the abstract point class.
concrete_point1 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # mutableX)
         .*. move_method self
         .*. p

concrete_point2 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. ((return 42):: IO Int)
         .*. move_method self
         .*. p

concrete_point3 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # mutableX)
         .*. move_method self
         .*. p


-- We compose a class which involves multiple inheritance.
-- An object of this class has *two* instances of abstract_point.
-- One of them is shared with concrete_point1  and concrete_point2,
-- and another is inherited from concrete_point3. Try this with C++!

heavy_point x_init color self =
  do
     super1 <- concrete_point1 x_init self
     super2 <- concrete_point2 x_init self 
     super3 <- mfix (concrete_point3 x_init)
     let myprint = do
                      putStr "super1: "; (super1 # print)
                      putStr "super2: "; (super2 # print)
                      putStr "super3: "; (super3 # print)
     let mymove  = ( \d -> do
                              super1 # moveX $ d
                              super2 # moveX $ d
                              super3 # moveX $ d )
     return 
       $    print  .=. myprint
      .*.   moveX   .=. mymove
      .*.   emptyRecord
      .<++. super1
      .<++. super2
      .<++. super3


myDiamondOOP =
  do 
     p <- mfix (heavy_point 42 "blue")
     p # print -- All points still agree!
     p # moveX $ 2
     p # print -- The third point lacks behind!

-- Note, try
-- :type heavy_point
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

-- That is not a problem in OOHaskell
-- If we do
-- :t printable_point
-- we see that our class is already polymorphic:
-- (..., Num a, ...) =>  a -> ...


main =
  do 
     putStrLn "mySelfishOOP"     ; mySelfishOOP
     putStrLn "myPolyOOP"        ; myPolyOOP
     putStrLn "myFirstClassOOP"  ; myFirstClassOOP printable_point
     putStrLn "myFirstClassOOP"  ; myFirstClassOOP $ flip colored_point' "red"
     putStrLn "myColoredOOP"     ; myColoredOOP
     putStrLn "myOverridingOOP"  ; myOverridingOOP
     putStrLn "testGeneric"      ; testGeneric
     putStrLn "testVirtual"      ; testVirtual
     putStrLn "testVirtual'"     ; testVirtual'
     putStrLn "testRestricted"   ; testRestricted
     putStrLn "testRestricted'"  ; testRestricted'
     putStrLn "myDiamondOOP"     ; myDiamondOOP


-- :t colored_point
-- :t mfix $ colored_point (1::Int) "red"