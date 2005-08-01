{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

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
import Record


-- Some labels for list operations

data IsEmpty; isEmpty  = proxy::Proxy IsEmpty
data GetHead; getHead  = proxy::Proxy GetHead
data GetTail; getTail  = proxy::Proxy GetTail
data SetHead; setHead  = proxy::Proxy SetHead
data InsHead; insHead  = proxy::Proxy InsHead


-- The interface of a recursive list

type ListInterface a =
     Record (     (Proxy IsEmpty  , IO Bool)
              :*: (Proxy GetHead  , IO a)
              :*: (Proxy GetTail  , IO (ListObj a))
              :*: (Proxy SetHead  , a -> IO ())
              :*: (Proxy InsHead  , a -> IO (ListObj a))
              :*: HNil )


-- A type distinction for List objects

newtype ListObj a =
        ListObj (ListInterface a)


-- Method access skips newtype

instance HasField l (ListInterface a) v =>
         HasField l (ListObj a) v
  where
  hLookupByLabel l (ListObj x) = 
      hLookupByLabel l x


-- The class for empty lists

nil_class (_::Proxy a) self
 = returnIO
     $  isEmpty  .=. returnIO True
    .*. getHead  .=. ((failIO "No head!")::IO a)
    .*. getTail  .=. ((failIO "No tail!")::IO (ListObj a))
    .*. setHead  .=. const ((failIO "No head!")::IO ())
    .*. insHead  .=. reusableInsHead self
    .*. emptyRecord


-- Reusable insert operation

reusableInsHead self (head::a)
 = do 
      newCons <- mfix (cons_class head self)
      returnIO ((ListObj newCons)::ListObj a)


-- The class for nonempty lists

cons_class head tail self
 = do
      hRef <- newIORef head
      returnIO
        $  isEmpty .=. returnIO False
       .*. getHead .=. readIORef hRef
       .*. getTail .=. returnIO (ListObj tail)
       .*. setHead .=. writeIORef hRef
       .*. insHead .=. reusableInsHead self
       .*. emptyRecord


-- Iteration for printing

printList aList
 = do
      empty <- aList # isEmpty
      if empty
        then putStrLn ""
        else ( do 
                  head <- aList # getHead
                  putStr $ show head
                  tail <- aList # getTail
                  putStr " "
                  printList tail
             )


-- Test case

main = do
          list1 <- mfix $ nil_class (proxy::Proxy Int)
          list2 <- (list1 # insHead) (88::Int)
          list3 <- (list2 # insHead) (41::Int)
          (list3 # setHead) (42::Int)
          printList list3
