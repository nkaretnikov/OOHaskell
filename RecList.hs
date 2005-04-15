{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004--2005, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

Illustration of recursive lists.

We use newtypes to make a type distinction for list objects.  This
illustrates nominal types for classes. In fact, we needed a type
distinction to encode ISO-recursive types. Note that we have added a
Hash instance so that "#" can be used as before.  Also note that we
have encoded immutable lists here, but this is a detail ... Well, in
fact, we have functional lists behind an OOish interface.

-}

module RecList where

import OOHaskell

infixr 9 #

m # field = (m .!. field) 


-- Some labels for list operations
data IsEmpty; isEmpty  = proxy::Proxy IsEmpty
data GetHead; getHead  = proxy::Proxy GetHead
data GetTail; getTail  = proxy::Proxy GetTail


-- The interface of a recursive list
type ListInterface a =
     Record (     (Proxy IsEmpty  , IO Bool)
              :*: (Proxy GetHead  , IO a)
              :*: (Proxy GetTail  , IO (ListObj a))
              :*: HNil )

-- A type distinction for List objects
newtype ListObj a =
        ListObj (ListInterface a)


-- Method access skips newtype
instance Hash l (ListInterface a) v =>
         Hash l (ListObj a) v
  where
    hLookupByLabel l (ListObj x) =
     hLookupByLabel l x


-- The class for empty lists
nil_class (_::Proxy a)
 = returnIO
     $  isEmpty  .=. returnIO True
    .*. getHead  .=. ((failIO "No head!")::IO a)
    .*. getTail  .=. ((failIO "No tail!")::IO (ListObj a))
    .*. emptyRecord


-- The class for nonempty lists
cons_class head tail
 = returnIO
     $  isEmpty .=. returnIO False
    .*. getHead .=. returnIO head
    .*. getTail .=. returnIO (ListObj tail)
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
                  printList $ tail
             )

main = do
          aNil   <- nil_class (proxy::Proxy Int)
          aCons1 <- cons_class (88::Int) aNil
          aCons2 <- cons_class (42::Int) aCons1
          printList (ListObj aCons2)
