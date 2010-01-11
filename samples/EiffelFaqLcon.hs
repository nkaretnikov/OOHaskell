{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell#-}
{-# LANGUAGE ScopedTypeVariables #-}

{-

-- (C) 2004-2005, Oleg Kiselyov & Ralf Laemmel
-- Haskell's overlooked object system

-- We demonstrate the encoding of covariance and its type safety in OOHaskell.
-- We adopt an example from http://www.faqs.org/faqs/eiffel-faq/
-- Cf. FAQ "LCON: Please explain and discuss covariance vs. contravariance."

-}


module EiffelFaqLcon where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)


{-

Quoted from the Eiffel FAQ: "Here's another example where a real-world
situation suggests a covariant solution. Herbivores eat plants. Cows
are herbivores. Grass is a plant. Cows eat grass but not other
plants."

   class HERBIVORE                               class PLANT
   feature
      eat(food: PLANT) is ...
      diet: LIST[PLANT]

   class COW                                     class GRASS
   inherit                                       inherit
      HERBIVORE                                     PLANT
         redefine eat
      end
   feature eat(food: GRASS) is ...

-}


-- Many labels

$(label "eat")
$(label "diet")
$(label "print")
$(label "meadow")


-- The plant base class

plant self 
  = do
       return
          $  print .=. putStr "PLANT"
         .*. emptyRecord
 

-- Grass -- a subclass of plant

grass (aMeadow::String) self 
  = do
       super <- plant self
       return
          $  print  .=. putStr "GRASS"
         .<. meadow .=. aMeadow -- Grass is more than plant.
         .*. super


-- The interface types for plant and grass

type IPlant 
 = Record ( Print :=: IO ()
         :*: HNil )

type IGrass
 = Record (  Print  :=: IO ()
         :*: Meadow :=: String
         :*: HNil )


-- The herbivore base class

herbivore self 
  = do
       return
          $  print .=. putStr "HERBIVORE"
         .*. eat   .=. (\food ->
                          do
                             -- We also document the argument type.
                             let (_::IPlant) = narrow food
                             self # print
                             putStr " eats "
                             food # print
                             putStr ".\n"
                       )
         .*. emptyRecord


-- A first test case -- no covariance yet

test1 = do
           Prelude.print "test1"
           --
           aPlant <- mfix $ plant
           aGrass <- mfix $ grass "Prairie"
           aHerb1 <- mfix $ herbivore
           aHerb2 <- mfix $ herbivore
           aHerb1 # eat $ aPlant -- eats fine
           aHerb2 # eat $ aGrass -- eats fine, too
           --
           -- Alas, herbs need to be forced to eat grass once they had plant.
           -- And we cannot even force them to eat plant once they had grass.
           -- This is due insufficient polymorphism for the do bindings.
           --
           aHerb1 # eat $ narrow aGrass    -- eat with force
           -- aHerb2 # eat $ narrow aPlant -- cannot eat in this type system
           --
           Prelude.print "OK"

{-

HERBIVORE eats PLANT.
HERBIVORE eats GRASS.
HERBIVORE eats GRASS.

-}


-- Cow -- a subclass of herbivore

cow self 
  = do
       super <- herbivore self
       return
          $  print .=. putStr "COW"
         .<. eat   .=. (\food ->
                          do
                             -- We assure the picky food constraint.
                             let (_::IGrass) = narrow food
                             super # eat $ food
                       )
         .<. super


-- A test case -- with covariance at work

test2 = do
           Prelude.print "test2"
           --
           aPlant <- mfix $ plant
           aGrass <- mfix $ grass "Prairie"
           aHerb  <- mfix $ herbivore
           aCow   <- mfix $ cow
           aHerb # eat $ aPlant
           -- aCow  # eat $ aPlant -- That would be a type error!
           aCow  # eat $ aGrass
           --
           Prelude.print "OK"

{-

HERBIVORE eats PLANT.
COW eats GRASS.

-}


{-

 The aforementioned Eiffel FAQ says:

 "The compiler must stop us from putting a COW object into a HERBIVORE
 attribute and trying to feed it a PLANT, but we shouldn't be trying
 to do this anyway."

 And indeed, OOHaskell does stop us.

-}

-- Let's try to place cows and herbivores together in a container.
-- We will see that the eat method prevents us from doing so.

test3 = do
           Prelude.print "test3"
           --
           aHerb  <- mfix herbivore
           aCow   <- mfix cow
           --
           -- This is still for preparation.
           -- Alas, we need to resolve the polymorphism of both guys.
           --
           aGrass <- mfix $ grass "any meadow"
           aHerb # eat $ aGrass
           aCow  # eat $ aGrass
           --
           -- Now we try to construct containers -- lists in fact.
           --
	   let herbList1 = [aHerb]
           --
	   -- let herbList2 = aCow : herbList1        -- Type error!
	   -- let herbList2 = narrow aCow : herbList1 -- Still type error!
           --
           -- We can treat non-eating cows and herbivores the same.
           --
	   let herbList3 = [aCow .-. eat, aHerb .-. eat]
	   mapM_ (\x -> do x # print; putStr "\n" ) herbList3
           --
           Prelude.print "OK"


main = do test1; test2; test3
