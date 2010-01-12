{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell#-}

{-

OOHaskell (C) 2004 -- 2007, Oleg Kiselyov, Ralf Laemmel

The OCaml tutorial's section on "Multiple inheritance" actually does
not illustrate multiple inheritance per se; it does show the
"inherits" notation though, which can be used to compose together
multiple heirs. Here is a challenging example of diamond inheritance
-- and a conversion of the open recursion into the closed recursion --
We implement the following diagram:

-}


module MultipleInheritance where

import OOHaskell
import OCamlTutorial hiding (main)
import Prelude hiding (print)

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



-- Methods as first class citizens

move_method self
 = moveX .=. (\d -> modifyIORef (self # varX) (+d))


-- The concrete classes derived from the abstract point class.

concrete_point1 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # varX)
         .*. move_method self
         .*. p

concrete_point2 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. ((return 88):: IO Int)
         .*. move_method self
         .*. p

concrete_point3 x_init self
   = do
        p <- abstract_point x_init self
        returnIO
          $  getX .=. readIORef (self # varX)
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
                      putStr "\nsuper1: "; (super1 # print)
                      putStr "\nsuper2: "; (super2 # print)
                      putStr "\nsuper3: "; (super3 # print)
                      putStr "\n"
     let mymove  = ( \d -> do
                              super1 # moveX $ d
                              super2 # moveX $ d
                              super3 # moveX $ d )
     return 
       $    print  .=. myprint
      .*.   moveX  .=. mymove
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

{-

super1: 42
super2: 42
super3: 42

super1: 46
super2: 46
super3: 44

-}

-- Note, try
-- :type heavy_point
-- The number of type variables is very impressive!


main = myDiamondOOP
