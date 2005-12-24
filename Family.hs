{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
{-# OPTIONS -fno-monomorphism-restriction #-}

module Family where

import OOHaskell


data Foo; foo = proxy::Proxy Foo
data Bar; bar = proxy::Proxy Bar
data Baz; baz = proxy::Proxy Baz


family1 =  foo .=. fooClass
       .*. emptyRecord

fooClass x self =
  returnIO
    $  bar .=. putStr ""
   .*. baz .=. x
   .*. emptyRecord


client1 family =
  do
     o1 <- mfix ((family # foo) True)
     o1 # bar
--   Sigh! Family monomorphic
--     o2 <- mfix ((family # foo) "True")
--     o2 # bar
     o3 <- mfix (fooClass True)
     o3 # bar
     o4 <- mfix (fooClass "True")
     o4 # bar

client2 fooClass = 
  do
     o5 <- mfix (fooClass True)
     o5 # bar
--   Sigh! Constructor monomorphic
--     o6 <- mfix (fooClass "True")
--     o6 # bar

main = do 
          client1 family1
          client2 fooClass
