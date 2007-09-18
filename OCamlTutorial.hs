{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fth #-}


{-

OOHaskell (C) 2004 -- 2007, Oleg Kiselyov, Ralf Laemmel

OOHaskell implementations of some excerpts from the OCaml Tutorial

@misc{OCaml,
 author = "Xavier Leroy and others",
 title  = "{The Objective Caml system, release 3.10, Documentation and user's manual}",
 year   = 2007,
 month  = "16~" # may,
 note   = "\url{http://caml.inria.fr/pub/docs/manual-ocaml/index.html}",
}

-}


module OCamlTutorial where

import OOHaskell
import Prelude hiding (print)

------------------------------------------------------------------------
--
-- Warm up for preparation of OCaml tutorial.
-- Simulating open products (records as in OCaml)
--

{-

In OCaml:

let foo f = f#fld1;;
val foo : < fld1 : 'a; .. > -> 'a = <fun>

-}

-- This is how we define labels.
data Field1 deriving Typeable; field1 = proxy::Proxy Field1

-- This is how record selection looks like.
foo f = f # field1

-- So this is how we actually demo record access.
testfoo  = foo (field1 .=. True .*. emptyRecord)


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.1 Classes and objects
--

{- Ocaml Tutorial:

The class point below defines one instance variable x and two methods
get_x and move. The initial value of the instance variable is 0. The
variable x is declared mutable, so the method move can change its
value.

 #class point =
    object
      val mutable varX = 0
      method getX = varX
      method moveX d = varX <- varX + d
    end;;
 class point :
   object val mutable varX : int method getX : int method moveX : int ->unit end

-}

-- First, declare the labels.

{-

-- We use proxies as of HList/Label4.hs

data VarX deriving Typeable; varX = proxy::Proxy VarX
data GetX  deriving Typeable;    getX     = proxy::Proxy GetX
data MoveX  deriving Typeable;    moveX     = proxy::Proxy MoveX

-- In fact, let's use the TH convenience for labels.

-}

$(label "varX")
$(label "getX")
$(label "moveX")


-- Note, here the field 'x' here is intentionally public -- just as in the
-- Ocaml code above

point = 
   do
      x <- newIORef 0
      return
        $  varX  .=. x
       .*. getX  .=. readIORef x
       .*. moveX .=. modifyIORef x . (+)
       .*. emptyRecord


{- Ocaml Tutorial:

 #let p = new point;;
 val p : point = <obj>

 Note that the type of p is point. This is an abbreviation
 automatically defined by the class definition above. It stands for
 the object type <getX : int; moveX : int -> unit>, listing the
 methods of class point along with their types.  We now invoke some
 methods to p:

 #p#getX;;
 - : int = 0

 #p#moveX 3;;
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
     p # getX >>= putStr . show
     p # moveX $ 3
     p # getX >>= putStr . show

-- The field varX is public and can be manipulated directly.

mySecondOOP =
  do 
     p <- point
     writeIORef (p # varX) 42
     p # getX >>= putStr . show



{- Ocaml Tutorial:

The class point can also be abstracted over the initial values of the
x coordinate.  The parameter x_init is, of course, visible in the
whole body of the definition, including methods. For instance, the
method getOffset in the class below returns the position of the
object relative to its initial position.

class para_point x_init =
   object
     val mutable varX    = x_init
     method getX      = x
     method getOffset = x - x_init
     method moveX d    = x <- x + d
   end;;

-}


-- A new label is needed for this example.
$(label "getOffset")


-- OCaml's parameterised class is nothing but a function.
para_point x_init =
   do
      x <- newIORef x_init
      return
        $  varX  .=. x
       .*. getX      .=. readIORef x
       .*. getOffset .=. queryIORef x (\v -> v - x_init)
       .*. moveX      .=. modifyIORef x . (+)
       .*. emptyRecord


-- A shortcut for IORef processing. Is that somewhere in the libraries?
queryIORef ref f = readIORef ref >>= returnIO . f

testPara =
   do
      p <- para_point 1
      p # getX >>= putStr . show
      p # moveX $ 2
      p # getX >>= putStr . show
      p # getOffset >>= putStr . show


-- We test the polymorphism of para_point
myPolyPara =
   do
      p  <- para_point (1::Int)
      p' <- para_point (1::Double)
      p  # moveX $ 2
      p' # moveX $ 2.5
      p  # getX >>= putStr . show
      p' # getX >>= putStr . show


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
     method moveX d    = x <- x + d
   end;;

This ability provides class constructors as can be found in other languages.
Several constructors can be defined this way to build objects of the same class
but with different initialization patterns; an alternative is to use
initializers, as decribed below in section 3.3.

-}

adjusted_point x_init =
   do
      let origin = (x_init `div` 10) * 10
      x <- newIORef origin
      return
        $  varX  .=. x
       .*. getX      .=. readIORef x
       .*. getOffset .=. queryIORef x (\v -> v - origin)
       .*. moveX     .=. modifyIORef x . (+)
       .*. emptyRecord

testConstr =
   do
      p <- adjusted_point 11
      p # getX >>= putStr . show
      p # moveX $ 2
      p # getX >>= putStr . show
      p # getOffset >>= putStr . show



{- Ocaml Tutorial:

The evaluation of the body of a class only takes place at object
creation time.  Therefore, in the following example, the instance
variable x is initialized to different values for two different
objects.

let x0 = ref 0;;
val x0 : int ref = {contents = 0}
 
class incrementing_point =
   object
     val mutable x = incr x0; !x0
     method getX  = x
     method moveX d = x <- x + d
   end;;
class incrementing_point :
  object val mutable x : int method getX : int method moveX : int -> unit end
 
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
      return (
        do
           modifyIORef x0 (+1)
           x <- readIORef x0 >>= newIORef
           return
             $  varX .=. x
            .*. getX     .=. readIORef x
            .*. moveX     .=. modifyIORef x . (+)
            .*. emptyRecord )


myNestedOOP =
   do
      localClass <- incrementing_point
      p1 <- localClass;
      p1 # getX  >>= putStr . show
      p2 <- localClass;
      p2 # getX  >>= putStr . show


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.3 Reference to self
--

-- Additional labels
$(label "print")


{- Ocaml Tutorial: 

A method or an initializer can send messages to self (that is, the
current object). For that, self must be explicitly bound, here to the
variable s (s could be any identifier, even though we will often
choose the name self.)

class printable_point x_init =
   object (s)
     val mutable varX = x_init
     method getX = x
     method moveX d = varX <- varX + d
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
      return
        $  varX  .=. x
       .*. getX  .=. readIORef x
       .*. moveX .=. modifyIORef x . (+)
--
-- To be revealed later
--     .*. print    .=. print_getX s
--
       .*. print .=. ((s # getX ) >>= putStr . show)
       .*. emptyRecord

-- We can share this print_getX method across all the objects
-- that have at least the method getX of type (Show a ) => IO a
-- The objects in question do not have to belong to the same hierarchy.
-- We re-use this function in abstract_point' below.

print_getX self = ((self # getX ) >>= putStr . show)

-- Note that 'mfix' plays the role of 'new' in the OCaml code...
mySelfishOOP =
   do
      p <- mfix (printable_point 7)
      p # moveX $ 2
      p # print


concrete_printable_point x_init 
  = concrete $ printable_point x_init


-- We test the polymorphism of printable_point
myPolyPrintable =
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



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.4 Initializers
--

{- Ocaml Tutorial:

Let-bindings within class definitions are evaluated before the object
is constructed. It is also possible to evaluate an expression
immediately after the object has been built. Such code is written as
an anonymous hidden method called an initializer. Therefore, is can
access self and the instance variables.

class initialized_point x_init =
   let origin = (x_init / 10) * 10 in
   object (self)
     val mutable varX = origin
     method getX = varX
     method moveX d = varX <- varX + d
     method print = print_int self#getX
     initializer print_string "new point at "; self#print;
   end;;

-}

-- We can model initializers just like in OCaml: use a dedicated label
-- `initializer'. We introduce a function `new' that, after doing mfix,
-- will locate the label `initializer' and run the corresponding action.
-- The anonymity of the initializer is achieved by removing it at the
-- end of the new sequence.


$(label "initializer")

initializable_point x_init self =
   do
      x <- newIORef x_init
      return
        $  varX        .=. x
       .*. getX        .=. readIORef x
       .*. moveX       .=. modifyIORef x . (+)
       .*. print       .=. ((self # getX ) >>= putStr . show)
       .*. initializer .=. do putStr "new point at "; self # print
       .*. emptyRecord


newAndInitialize og =
   do
      o <- mfix og
      o # initializer
      return $ o .-. initializer 


myInitializingOOP =
   do
      p <- newAndInitialize (initializable_point 7)
      p # moveX $ 2
      p # print


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.8 Inheritance
--

{- Ocaml Tutorial:

We illustrate inheritance by defining a class of colored points that
inherits from the class of points. This class has all instance
variables and all methods of class point, plus a new instance variable
c and a new method color.

class colored_point x (c : string) =
   object
     inherit point x
     val c = c
     method getColor = c
   end;;

let p' = new colored_point 5 "red";;
val p' : colored_point = <obj>
 
p'#getX, p'#getColor;;
- : int * string = (5, "red")

-}


$(label "getColor")


-- Inheritance is simple: just adding methods ...
colored_point x_init (color::String) self =
   do
        super <- printable_point x_init self
        return
            $  getColor .=. (returnIO color)
           .*. super


myColoredOOP =
   do
      p' <- mfix (colored_point 5 "red")
      x  <- p' # getX
      c  <- p' # getColor
      putStr $ show (x,c)


-- We derive a better class of colored points, which prints more accurately.
-- To this end, we access the overriden method akin to the OCaml super.

colored_point' x_init (color::String) self =
   do
      super <- colored_point x_init color self
      return $  print .=. (
                  do  putStr "so far - "; super # print
                      putStr "color  - "; putStr color )
            .<. super

myOverridingOOP =
   do
      p  <- mfix (colored_point' 5 "red")
      p  # print



myOverridingOOP' = testPointDouble (\d -> colored_point' d "red")


{- Ocaml Tutorial:

A point and a colored point have incompatible types, since a point has
no method color. However, the function getX below is a generic
function applying method getX to any object p that has this method
(and possibly some others, which are represented by an ellipsis in the
type). Thus, it applies to both points and colored points.

#let get_succ_x p = p#getX + 1;;
val get_succ_x : < getX : int; .. > -> int = <fun>

#let p = new printable_point 7;;
val p : printable_point = <obj>

#let p' = new colored_point 5 "red";;
val p' : colored_point = <obj>

#get_succ_x p + get_succ_x p';;
- : int = 14

-}


get_succ_x p = p # getX >>= (return . (+ 1))

myPolymorphicOOP =
   do
      p  <- mfix (printable_point 7)
      p' <- mfix (colored_point 5 "red")
      x  <- get_succ_x p
      x' <- get_succ_x p'
      putStr $ show (x+x') -- prints 14




------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.9 Multiple inheritance
--

{- Ocaml Tutorial:

Multiple inheritance is allowed. Only the last definition of a method
is kept: the redefinition in a subclass of a method that was visible
in the parent class overrides the definition in the parent
class. Previous definitions of a method can be reused by binding the
related ancestor. Below, super is bound to the ancestor
printable_point. The name super is a pseudo value identifier that can
only be used to invoke a super-class method, as in super#print.

class printable_colored_point x_init c =
   object (self)
     val c = c
     method getColor = c
     inherit printable_point x_init as super
     method print =
       print_string "(";
       super#print;
       print_string ", ";
       print_string (self#getColor);
       print_string ")"
   end;;

-}


printable_colored_point y c self =
   do
      super <- printable_point y self
      return
        $  print    .=. ( do 
                             putStr "("
                             super # print
                             putStr ", "
                             self # getColor >>= putStr
                             putStr ")" )
       .<. getColor .=. return c 
       .*. super

myOverridingOOP'' =
   do
      p <- mfix (printable_colored_point 5 "red")
      p # print



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.5 Virtual methods
--

{- Ocaml Tutorial:

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

abstract_point x_init self 
  | const False ( (narrow self) `asTypeOf` desired_type x_init )
  = undefined
 where 
 desired_type :: a -> Record (  GetX  :=: IO a
				:*: MoveX :=: (a -> IO ())
				:*: HNil )
 desired_type = undefined

abstract_point x_init self =
   do
      xRef <- newIORef x_init
      returnIO $
           varX  .=. xRef
       .*. print     .=. (self # getX >>= putStr . show )
       .*. emptyRecord

 -- This is an optional part in case we want to fix types of virtuals.
 where
  _ = narrow self `asTypeOf` desired_type x_init

  desired_type :: a -> Record (  GetX  :=: IO a
				:*: MoveX :=: (a -> IO ())
				:*: HNil )
  desired_type = undefined

concrete_point x_init self
   = do
        p <- abstract_point x_init self -- inherit ...
        returnIO
        -- add the missing (pure virtual) methods
          $  getX  .=. readIORef (self # varX)
         .*. moveX .=. (\d -> modifyIORef (self # varX) (+d))
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
        p # getX >>= putStr . show
        p # moveX $ 2
        p # getX >>= putStr . show
        p # print


-- This abstract point class mentions the type of the virtual methods.

abstract_point' x_init self
  = do
      x <- newIORef x_init
      returnIO $
	   varX  .=. x
       .*. getX  .=. (proxy::Proxy (IO Int))
       .*. moveX .=. (proxy::Proxy (Int -> IO ()))
       .*. print .=. print_getX self -- now we reuse this function
       .*. emptyRecord


-- Another label for testing purposes
data MyLabel; myLabel = proxy::Proxy MyLabel


-- This concrete class implements all virtual methods
concrete_point' x_init self
   = do
        p <- abstract_point' x_init self -- inherit ...
        returnIO
        -- use disciplined record update
           $  getX    .=. readIORef (self # varX)
          .^. moveX   .=. modifyIORef (self # varX) . (+)
          .^. myLabel .=. ()                -- This line could be activated.
--        .^. myLabel .=. (proxy::Proxy ()) -- A proxy that disables mnew.
          .*. p

-- We introduce a constrained new method to refuse proxy fields in records.
mnew f = mfix f
 where
  () = hasNoProxies (get_class_type f) 
  get_class_type:: (a->m a) -> a
  get_class_type = undefined

testVirtual'
   = do
        p <- mnew (concrete_point' 7)
        p # getX >>= putStr . show
        p # moveX $ 2
        p # getX >>= putStr . show
        p # print


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.6 Private methods
--

{- Ocaml Tutorial:

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


-- Private methods are modelled by let bindings.
-- So they are not put into the record of an object.
-- We could achieve sharing of methods between different objects via lets.

$(label "bumpX")

restricted_point x_init self =
   do
      x <- newIORef x_init
      let moveX = modifyIORef x . (+)
      return 
        $  varX .=. x
       .*. getX     .=. readIORef x
       .*. bumpX    .=. moveX 2
       .*. emptyRecord

testRestricted
   =  do
       p <- mfix (restricted_point 7)
       p # getX >>= putStr . show
       p # bumpX
       p # getX >>= putStr . show


-- Unlike the OCaml code, we can also remove a method from the interface.
-- This allows us to make methods private for existing objects.
-- We first add the bump method that uses the private method moveX.

bumping_point x_init self =
   do
      p <- printable_point x_init self
      return
        $  bumpX .=. (self # moveX $ 2)
       .*. p

testRestricted' = 
   do
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



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.10 Parameterized classes
--

{- Ocaml Tutorial: 3.10 Parameterized classes

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




main = do 
          putStrLn "myFirstOOP"; myFirstOOP; putStrLn ""
          putStrLn "mySecondOOP"; mySecondOOP; putStrLn ""
          putStrLn "testPara"; testPara; putStrLn ""
          putStrLn "testConstr"; testConstr; putStrLn ""
          putStrLn "myNestedOOP"; myNestedOOP; putStrLn ""
          putStrLn "mySelfishOOP"; mySelfishOOP; putStrLn ""
          putStrLn "myPolyPara"; myPolyPara; putStrLn ""
          putStrLn "myPolyPrintable"; myPolyPrintable; putStrLn ""
          putStrLn "myFirstClassOOP"; myFirstClassOOP printable_point
          putStrLn "myFirstClassOOP"
          myFirstClassOOP $ flip colored_point' "red"
          putStrLn ""
          putStrLn "myColoredOOP"; myColoredOOP; putStrLn ""
          putStrLn "myOverridingOOP"; myOverridingOOP; putStrLn ""
          putStrLn "myOverridingOOP'"; myOverridingOOP'; putStrLn ""
          putStrLn "myOverridingOOP''"; myOverridingOOP''; putStrLn ""
          putStrLn "myPolymorphicOOP"; myPolymorphicOOP; putStrLn ""
          putStrLn "testVirtual"; testVirtual; putStrLn ""
          putStrLn "testVirtual'"; testVirtual'; putStrLn ""
          putStrLn "testRestricted"; testRestricted; putStrLn ""
          putStrLn "testRestricted'"; testRestricted'; putStrLn ""

-- :t colored_point
-- :t mfix $ colored_point (1::Int) "red"
