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
      p <- point
      p # getX >>= putStrLn . show
      p # moveX $ 3
      p # getX >>= putStrLn . show

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
        $  varX       .=. x
       .*. getX       .=. readIORef x
       .*. getOffset  .=. queryIORef x (\v -> v - x_init)
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
myBoundedGenericClassOOP =
   do
      p  <- para_point (1::Int)
      p' <- para_point (1::Double)
      p  # moveX $ 2
      p' # moveX $ 2.5
      -- p  # moveX $ 2.5 -- type error!
      p  # getX >>= putStrLn . show -- prints 3
      p' # getX >>= putStrLn . show -- prints 3.5



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
        $  varX      .=. x
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
             $  varX  .=. x
            .*. getX  .=. readIORef x
            .*. moveX .=. modifyIORef x . (+)
            .*. emptyRecord )


myNestedOOP =
   do
      classObject <- incrementing_point
      p1 <- classObject;
      p1 # getX  >>= putStrLn . show
      p2 <- classObject;
      p2 # getX  >>= putStrLn . show


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
       .*. print .=. ((s # getX) >>= putStr . show)
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

class initializable_point x_init =
   let origin = (x_init / 10) * 10 in
   object (self)
     val mutable varX = origin
     method getX      = varX
     method moveX d   = varX <- varX + d
     method print     = print_int self#getX
     initializer print_string "new point at "; self#print
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
       .*. initializer .=. do putStr "new point at "; self # print; putStrLn ""
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


initializable_point' x_init self =
   do
      x <- newIORef x_init
      return (
            varX        .=. x
       .*. getX        .=. readIORef x
       .*. moveX       .=. modifyIORef x . (+)
       .*. print       .=. ((self # getX ) >>= putStr . show)
       .*. emptyRecord
        , Just $ do putStr "new point at "; self # print; putStrLn "" )


newAndInitialize' og =
   do
      (o,i) <- mfix (og . fst)
      case i of Nothing -> return (); (Just i') -> i'
      return o


myInitializingOOP' =
   do
      p <- newAndInitialize' (initializable_point' 7)
      p # moveX $ 2
      p # print



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.8 Inheritance
--

{- Ocaml Tutorial:

We illustrate inheritance by defining a class of colored points that
inherits from the class of printable points. This class has all
instance variables and all methods of class point, plus a new instance
variable c and a new method color.

class colored_point x_init (c : string) =
   object
     inherit printable_point x_init
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
colored_point x_init (c::String) self =
   do
      super <- printable_point x_init self
      return
        $  getColor .=. returnIO c
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

myGenericFunctionOOP =
   do
      p  <- mfix (printable_point 7)
      p' <- mfix (colored_point 5 "red")
      x  <- get_succ_x p
      x' <- get_succ_x p'
      putStrLn $ show (x+x') -- prints 14




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


printable_colored_point x_init c self =
   do
      super <- printable_point x_init self
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

{-

-- One way to fix the type of an abstract method.

abstract_point x_init self 
  | const False ( (narrow self) `asTypeOf` desired_type x_init )
  = undefined
 where 
 desired_type :: a -> Record (  GetX  :=: IO a
				:*: MoveX :=: (a -> IO ())
				:*: HNil )
 desired_type = undefined

-}

{-

-- Another way to fix the type of an abstract method.

abstract_point x_init self =
   do
      xRef <- newIORef x_init
      return 
        $  varX  .=. xRef
       .*. print .=. (self # getX >>= putStr . show)
       .*. emptyRecord
   where
     _ = constrain x_init self
     constrain :: ( HasField (Proxy GetX)  self (IO x_init)
                  , HasField (Proxy MoveX) self (x_init -> IO ())
                  ) => x_init -> self -> ()
     constrain _ _ = ()

-}

-- Yet another way to fix the type of an abstract method.

abstract_point x_init self =
   do
      xRef <- newIORef x_init
      return 
        $  varX  .=. xRef
       .*. print .=. (self # getX >>= putStr . show)
       .*. emptyRecord
   where
     _ = (self # getX) `asTypeOf` returnIO x_init
     _ = (self # moveX) x_init `asTypeOf` returnIO ()


{-

-- Without enforcement

abstract_point x_init self =
   do
      xRef <- newIORef x_init
      return 
        $  varX  .=. xRef
       .*. print .=. (self # getX >>= putStr . show)
       .*. emptyRecord

-}

{-

-- Another way to fix the type of an abstract method.

 where
  _ = narrow self `asTypeOf` desired_type x_init

  desired_type :: a -> Record (  GetX  :=: IO a
				:*: MoveX :=: (a -> IO ())
				:*: HNil )
  desired_type = undefined

-}

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

$(label "bump")

restricted_point x_init self =
   do
      x <- newIORef x_init
      let moveX = modifyIORef x . (+)
      return 
        $  varX .=. x
       .*. getX     .=. readIORef x
       .*. bump     .=. moveX 2
       .*. emptyRecord

myPrivacyOOP
   =  do
       p <- mfix (restricted_point 7)
       p # getX >>= putStrLn . show -- prints 7
       p # bump
       p # getX >>= putStrLn . show -- prints 9


-- Unlike the OCaml code, we can also remove a method from the interface.
-- This allows us to make methods private for existing objects.
-- We first add the bump method that uses the private method moveX.

bumping_point x_init self =
   do
      p <- printable_point x_init self
      return
        $  bump .=. (self # moveX $ 2)
       .*. p

myPrivacyOOP' = 
   do
      p  <- mfix (bumping_point 7)
      let p' = p .-. moveX
      p' # print
      p' # bump
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

{- Ocaml Tutorial:

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

$(label "var")
$(label "get")
$(label "set")

ref init = 
   do
      varRef <- newIORef init
      return
        $  var .=. varRef
       .*. get .=. readIORef varRef
       .*. set .=. writeIORef varRef
       .*. emptyRecord


myGenericClassOOP =
   do
      r <- ref 1
      r # set $ 2
      r # get >>= putStrLn . show -- prints 2


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.11 Polymorphic methods
--

{- Ocaml Tutorial:

While parameterized classes may be polymorphic in their contents, they
are not enough to allow polymorphism of method use.

A classical example is defining an iterator.

#List.fold_left;;
- : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a = <fun>
 
#class ['a] intlist (l : int list) =
   object
     method empty = (l = [])
     method fold f (z : 'a) = List.fold_left f z l
   end;;
class ['a] intlist :
  int list ->
  object method empty : bool method fold : ('a -> int -> 'a) -> 'a -> 'a end

At first look, we seem to have a polymorphic iterator, however this
does not work in practice.

#let l = new intlist [1; 2; 3];;
val l : '_a intlist = <obj>
 
#l#fold (fun x y -> x+y) 0;;
- : int = 6
 
#l;;
- : int intlist = <obj>
 
#l#fold (fun s x -> s ^ string_of_int x ^ " ") "";;
This expression has type int but is here used with type string

Our iterator works, as shows its first use for summation. However,
since objects themselves are not polymorphic (only their constructors
are), using the fold method fixes its type for this individual
object. Our next attempt to use it as a string iterator fails.

The problem here is that quantification was wrongly located: this is
not the class we want to be polymorphic, but the fold method. This can
be achieved by giving an explicitly polymorphic type in the method
definition.

#class intlist (l : int list) =
   object
     method empty = (l = [])
     method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a =
       fun f z -> List.fold_left f z l
   end;;
class intlist :
  int list ->
  object method empty : bool method fold : ('a -> int -> 'a) -> 'a -> 'a end
 
#let l = new intlist [1; 2; 3];;
val l : intlist = <obj>
 
#l#fold (fun x y -> x+y) 0;;
- : int = 6
 
#l#fold (fun s x -> s ^ string_of_int x ^ " ") "";;
- : string = "1 2 3 "

-}

$(label "empty")
$(label "fold")


intlist (l::[Int]) = 
                      empty .=. null l
                  .*. fold    .=. (\f z -> foldl f z l)
                  .*. emptyRecord


myGenericMethodOOP =
   do
      let l = intlist [1,2,3]
      putStrLn $ show $ (l # fold) (+) 0
      putStrLn $ (l # fold) (\s x -> s ++ show x ++ " ") ""


intlist' (l::[Int]) =
   do
      return 
         $  empty .=. null l
        .*. fold  .=. (\f z -> foldl f z l)
        .*. emptyRecord

myGenericMethodOOP' =
   do
      l <- intlist' [1,2,3]
      putStrLn $ show $ (l # fold) (+) 0
--      putStrLn $ (l # fold) (\s x -> s ++ show x ++ " ") ""


intlist'' (l::[Int]) =
   do
      return 
         $ empty .=. null l
        .*. fold    .=. FoldMethod (\f z -> foldl f z l)
        .*. emptyRecord


newtype FoldMethod =
        FoldMethod { foldMethod :: forall a. (a -> Int -> a) -> a -> a }


myGenericMethodOOP'' =
   do
      l <- intlist'' [1,2,3]
      putStrLn $ show $ foldMethod (l # fold) (+) 0
      putStrLn $ foldMethod (l # fold) (\s x -> s ++ show x ++ " ") ""


------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.16  Binary methods
--

{- Ocaml Tutorial:

A binary method is a method which takes an argument of the same type
as self. The class comparable below is a template for classes with a
binary method leq of type 'a -> bool where the type variable 'a is
bound to the type of self. Therefore, #comparable expands to < leq :
'a -> bool; .. > as 'a. We see here that the binder as also allows to
write recursive types.

#class virtual comparable = 
   object (_ : 'a)
     method virtual leq : 'a -> bool
   end;;
class virtual comparable : object ('a) method virtual leq : 'a -> bool end

We then define a subclass money of comparable. The class money simply
wraps floats as comparable objects. We will extend it below with more
operations. There is a type constraint on the class parameter x as the
primitive <= is a polymorphic comparison function in Objective
Caml. The inherit clause ensures that the type of objects of this
class is an instance of #comparable.

#class money (x : float) =
   object
     inherit comparable
     val repr = x
     method value = repr
     method leq p = repr <= p#value
   end;;
class money :
  float ->
  object ('a)
    val repr : float
    method leq : 'a -> bool
    method value : float
  end

Note that the type money1 is not a subtype of type comparable, as the
self type appears in contravariant position in the type of method
leq. Indeed, an object m of class money has a method leq that expects
an argument of type money since it accesses its value
method. Considering m of type comparable would allow to call method
leq on m with an argument that does not have a method value, which
would be an error.

-}


$(label "leq")
$(label "value")

--
-- A naive attempt that overlocks the need for recursive types
--

comparable self =
   do
        return emptyRecord
   where
     _ = (self # leq $ self) :: Bool


money (x::Float) self =
   do
      super <- comparable self
      return 
        $  value .=. x
       .*. leq .=. (\p -> self # value <= p # value)
       .*. super

{-

-- Type checking runs into occurs check!

myBinaryMethodOOP =
   do
        m <- mfix $ money 42.88 -- occurs check complains
        m' <- mfix $ money 88.42 -- occurs check complains
        putStrLn $ show (m # leq $ m') -- should print True
        putStrLn $ show (m' # leq $ m) -- should print False

-}



--
-- An option w/ an iso-recursive type, w/ tail polymorphism
--

newtype Comparable tail = 
        Comparable { getComparableRecord :: ComparableRecord tail }


type ComparableRecord tail = 
     Record ( Leq :=: (Comparable tail -> Bool) :*: tail )


instance HasField l (ComparableRecord tail) v
      => HasField l (Comparable tail) v
   where
     hLookupByLabel l = hLookupByLabel l . getComparableRecord


money' (x::Float) self =
   do
      return $ Comparable (
                    leq   .=. (\p -> self # value <= p # value)
                .*. value .=. x
                .*. emptyRecord )


myBinaryMethodOOP' =
   do
        m <- mfix $ money' 42.88
        m' <- mfix $ money' 88.42
        putStrLn $ show (m # leq $ m') -- prints True
        putStrLn $ show (m' # leq $ m) -- prints False



--
-- An option w/ an iso-recursive type, w/o inheritance
--


newtype Money = 
        Money { getMoneyRecord :: MoneyRecord }


type MoneyRecord = Record (  Value :=: Float
                         :*: Leq   :=: (Money -> Bool)
                         :*: HNil )


instance HasField l MoneyRecord v
      => HasField l Money v
 where
  hLookupByLabel l = hLookupByLabel l . getMoneyRecord


money'' (x::Float) self =
   do
      return $ Money (  value .=. x
                    .*. leq   .=. (\p -> self # value <= p # value)
                    .*. emptyRecord )

      
myBinaryMethodOOP'' =
   do
        m <- mfix $ money'' 42.88
        m' <- mfix $ money'' 88.42
        putStrLn $ show (m # leq $ m') -- prints True
        putStrLn $ show (m' # leq $ m) -- prints False



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.7 Class interfaces
--

{- Ocaml Tutorial:

Class interfaces are inferred from class definitions. They may also be
defined directly and used to restrict the type of a class. Like class
declarations, they also define a new type abbreviation.

#class type restricted_point_type = 
   object
     method get_x : int
     method bump : unit
 end;;
class type restricted_point_type =
  object method bump : unit method get_x : int end
 
#fun (x : restricted_point_type) -> x;;
- : restricted_point_type -> restricted_point_type = <fun>

In addition to program documentation, class interfaces can be used to
constrain the type of a class. Both concrete instance variables and
concrete private methods can be hidden by a class type
constraint. Public methods and virtual members, however, cannot.

#class restricted_point' x = (restricted_point x : restricted_point_type);;
class restricted_point' : int -> restricted_point_type

Or, equivalently:

#class restricted_point' = (restricted_point : int -> restricted_point_type);;
class restricted_point' : int -> restricted_point_type

-}

type Restricted_point_type = 
     Record (  GetX :=: IO Int
           :*: Bump :=: IO ()
           :*: HNil )


doBump :: Restricted_point_type -> IO ()
doBump x = x # bump

doBump' x = x # bump
   where
     _ = constrain $ narrow x 
     constrain :: Restricted_point_type -> ()
     constrain _ = ()


restricted_point' :: Int -> IO Restricted_point_type
restricted_point' x_init =
   do
      p <- mfix $ restricted_point x_init
      return $ narrow p


myTypedOOP =
   do
      p <- restricted_point' 42
      doBump p
      p # getX >>= putStrLn . show
      p' <- mfix $ restricted_point 42
      doBump' p
      p # getX >>= putStrLn . show


-- Try the polymorphic type
type Restricted_point_type' t = 
     Record (  GetX :=: IO t
           :*: Bump :=: IO ()
           :*: HNil )


-- HasField constraint is still not met
-- doBump'' :: Num x => Restricted_point_type' x -> IO ()
-- doBump'' x = x # bump


doBump'' x = x # bump
   where
     _ = constrain $ narrow x 
     constrain :: Restricted_point_type' t -> ()
     constrain _ = ()


myTypedOOP' =
   do
      p <- mfix $ restricted_point 42
      doBump'' p
      p # getX >>= putStrLn . show



------------------------------------------------------------------------
--
-- OCaml Objects tutorial
-- Sec 3.12 Using coercions
--

{- Ocaml Tutorial:

Subtyping is never implicit. There are, however, two ways to perform
subtyping. The most general construction is fully explicit: both the
domain and the codomain of the type coercion must be given.

We have seen that points and colored points have incompatible
types. For instance, they cannot be mixed in the same list. However, a
colored point can be coerced to a point, hiding its color method:

#let colored_point_to_point cp = (cp : colored_point :> point);;
val colored_point_to_point : colored_point -> point = <fun>
 
#let p = new point 3 and q = new colored_point 4 "blue";;
val p : point = <obj>
val q : colored_point = <obj>
 
#let l = [p; (colored_point_to_point q)];;
val l : point list = [<obj>; <obj>]

An object of type t can be seen as an object of type t' only if t is a
subtype of t'. For instance, a point cannot be seen as a colored
point.

#(p : point :> colored_point);;
Type point = < get_offset : int; get_x : int; move : int -> unit >
is not a subtype of type
  colored_point =
    < color : string; get_offset : int; get_x : int; move : int -> unit > 

Indeed, narrowing coercions without runtime checks would be
unsafe. Runtime type checks might raise exceptions, and they would
require the presence of type information at runtime, which is not the
case in the Objective Caml system. For these reasons, there is no such
operation available in the language.

Be aware that subtyping and inheritance are not related. Inheritance
is a syntactic relation between classes while subtyping is a semantic
relation between types. For instance, the class of colored points
could have been defined directly, without inheriting from the class of
points; the type of colored points would remain unchanged and thus
still be a subtype of points.

-}


type Point x = 
     Record (  GetX  :=: IO x
           :*: MoveX :=: (x -> IO ())
           :*: Print :=: IO ()
           :*: HNil )

to_point p = p'
 where
   p' = narrow p
   _  = constrain p' 
   constrain :: Point x -> ()
   constrain _ = ()

myCoercingOOP =
   do
      p  <- mfix $ printable_point 42
      p' <- mfix $ colored_point 88 "red"
      let l = [to_point p, to_point p']
      (l!!1) # print



------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------



main = do 
          putStrLn "myFirstOOP"; myFirstOOP; putStrLn ""
          putStrLn "mySecondOOP"; mySecondOOP; putStrLn ""
          putStrLn "testPara"; testPara; putStrLn ""
          putStrLn "testConstr"; testConstr; putStrLn ""
          putStrLn "myNestedOOP"; myNestedOOP; putStrLn ""
          putStrLn "myInitializingOOP"; myInitializingOOP; putStrLn ""
          putStrLn "myInitializingOOP'"; myInitializingOOP'; putStrLn ""
          putStrLn "mySelfishOOP"; mySelfishOOP; putStrLn ""
          putStrLn "myBoundedGenericClassOOP"; myBoundedGenericClassOOP; putStrLn ""
          putStrLn "myPolyPrintable"; myPolyPrintable; putStrLn ""
          putStrLn "myFirstClassOOP"; myFirstClassOOP printable_point
          putStrLn "myFirstClassOOP"
          myFirstClassOOP $ flip colored_point' "red"
          putStrLn ""
          putStrLn "myColoredOOP"; myColoredOOP; putStrLn ""
          putStrLn "myOverridingOOP"; myOverridingOOP; putStrLn ""
          putStrLn "myOverridingOOP'"; myOverridingOOP'; putStrLn ""
          putStrLn "myOverridingOOP''"; myOverridingOOP''; putStrLn ""
          putStrLn "myGenericFunctionOOP"; myGenericFunctionOOP; putStrLn ""
          putStrLn "myGenericClassOOP"; myGenericClassOOP; putStrLn ""
          putStrLn "myGenericMethodOOP"; myGenericMethodOOP
          putStrLn "myGenericMethodOOP'"; myGenericMethodOOP'
          putStrLn "myGenericMethodOOP''"; myGenericMethodOOP''
          putStrLn "testVirtual"; testVirtual; putStrLn ""
          putStrLn "testVirtual'"; testVirtual'; putStrLn ""
          putStrLn "myPrivacyOOP"; myPrivacyOOP; putStrLn ""
          putStrLn "myPrivacyOOP'"; myPrivacyOOP'; putStrLn ""
          putStrLn "myBinaryMethodOOP'"; myBinaryMethodOOP'
          putStrLn "myBinaryMethodOOP''"; myBinaryMethodOOP''

-- :t colored_point
-- :t mfix $ colored_point (1::Int) "red"
