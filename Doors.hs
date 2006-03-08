{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

Example from "Classes and mixins"

Run it like this:

> ghci -iHList -iOOHaskell door2.hs

Make sure HList and OOHaskell are checked out in src.

-}

module Doors where

import OOHaskell


-- All the labels that are needed

data CanOpen; canOpen = proxy::Proxy CanOpen
data CanPass; canPass = proxy::Proxy CanPass


-- Items and persons, not using OOHaskell

data Item   = TheKey deriving (Eq)
data Person = Person [Item] Float

hasItem :: Person -> Item -> Bool
hasItem (Person is _) i = elem i is

height :: Person -> Float
height (Person _ h)   = h


-- The base class for doors

doorC self = 
  do
     returnIO
        $  canOpen .=. (\(p::Person) -> returnIO True)
       .*. canPass .=. (\(p::Person) -> returnIO True)
       .*. emptyRecord


-- The mixin for locked doors

lockedM x self =
  do
     super <- x self
     returnIO
        $  canOpen .=. (\(p::Person) -> do
             b <- super # canOpen $ p
             returnIO (hasItem p TheKey && b)
           )
       .<. super


-- The mixin for short doors

shortM x self =
  do
     super <- x self
     returnIO
        $  canPass .=. (\(p::Person) -> do
             b <- super # canPass $ p
             returnIO (height p <= 1 && b)
           )
       .<. super


-- Test harness

main = do

  -- Subjects
  let p1 = Person [] 0       -- plain person
  let p2 = Person [TheKey] 0 -- person with key
  let p3 = Person [] 2       -- tall person

  -- Plain doors
  d1 <- mfix doorC
  (d1 # canOpen) p1 >>= print
  (d1 # canPass) p1 >>= print

  -- Locked doors
  d2 <- mfix (lockedM doorC)
  (d2 # canOpen) p1 >>= print
  (d2 # canOpen) p2 >>= print

  -- Short doors
  d3 <- mfix (shortM doorC)
  (d3 # canPass) p1 >>= print
  (d3 # canPass) p3 >>= print

  -- Short doors that are also locked
  d4 <- mfix (lockedM (shortM doorC))
  (d4 # canOpen) p1 >>= print
  (d4 # canPass) p1 >>= print
  (d4 # canOpen) p2 >>= print
  (d4 # canPass) p2 >>= print
  (d4 # canOpen) p3 >>= print
  (d4 # canPass) p3 >>= print
