{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-

OOHaskell (C) 2004--2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

Illustration of recursive lists.

We use newtypes to make a type distinction for list objects. This
illustrates nominal types for classes. In fact, we needed a type
distinction to encode ISO-recursive types. Note that we have added a
HasField instance so that "#" can be used as before.

-}


module RecList where

import OOHaskell


-- Some labels for list operations

data IsEmpty; isEmpty  = proxy::Proxy IsEmpty
data GetHead; getHead  = proxy::Proxy GetHead
data GetTail; getTail  = proxy::Proxy GetTail
data SetHead; setHead  = proxy::Proxy SetHead
data InsHead; insHead  = proxy::Proxy InsHead


-- The interface of a recursive list

type ListInterface a =
     Record (     IsEmpty :=: Bool
              :*: GetHead :=: IO a
              :*: GetTail :=: IO (ListObj a)
              :*: SetHead :=: (a -> IO ())
              :*: InsHead :=: (a -> IO (ListObj a))
              :*: HNil )


-- A type distinction for List objects

newtype ListObj a =
        ListObj (ListInterface a)


-- Method access skips newtype

instance HasField l (ListInterface a) v =>
         HasField l (ListObj a) v
  where
  hLookupByLabel l (ListObj x) = x # l


-- The class for empty lists
nilOO self | False = undefined :: IO (ListInterface a)
nilOO self
 = return
     $  isEmpty  .=. True
    .*. getHead  .=. fail "No head!"
    .*. getTail  .=. fail "No tail!"
    .*. setHead  .=. const (fail "No head!")
    .*. insHead  .=. reusableInsHead self
    .*. emptyRecord


-- Reusable insert operation

reusableInsHead list head
 = do 
      newCons <- mfix (consOO head list)
      return (ListObj newCons)


-- The class for nonempty lists

consOO head tail self
 = do
      hRef <- newIORef head
      return
        $  isEmpty .=. False
       .*. getHead .=. readIORef hRef
       .*. getTail .=. return (ListObj tail)
       .*. setHead .=. writeIORef hRef
       .*. insHead .=. reusableInsHead self
       .*. emptyRecord


-- Iteration for printing

printList aList
 = do
      if aList # isEmpty
        then putStrLn ""
        else do 
                head <- aList # getHead
                putStr $ show head
                tail <- aList # getTail
                putStr " "
                printList tail


-- Test case

main = do
          list1 <- mfix $ nilOO 
          list2 <- (list1 # insHead) (88::Int)
          list3 <- (list2 # insHead) (41::Int)
          (list3 # setHead) (42::Int)
          printList list3
