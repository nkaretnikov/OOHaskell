{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}
-- {-# OPTIONS -fno-monomorphism-restriction #-}

module Family where

import OOHaskell


data Foo; foo = proxy::Proxy Foo
data Bar; bar = proxy::Proxy Bar
data Baz; baz = proxy::Proxy Baz
data Qux; qux = proxy::Proxy Qux


family1 =  foo .=. fooClass
       .*. emptyRecord

fooClass x self =
  returnIO
    $  bar .=. (putStrLn $ show $ (self # baz))
   .*. baz .=. x
   .*. emptyRecord

fooClass2 x y self =
  do
  super <- fooClass x self
  returnIO
    $  qux .=. y
   .*. (bar .=. (putStrLn ("fooClass2" ++ (show (self # qux))) >> super # bar)
	.<. super)


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

family2 fooself =  foo .=. (\x -> fooClass x fooself)
       .*. emptyRecord

-- To compile the latter, we should NOT have -fno-monomorphism-restriction
-- Monomorphism restriction is good here!
client1' =
  do
     putStrLn "client1'"
     o1 <- mfix (\fooself -> ((family2 fooself) # foo) True)
     o1 # bar
     o2 <- mfix (\fooself -> ((family2 fooself) # foo) "True")
     o2 # bar

-- Note that when it comes to class constructors, we should try to find
-- the greatest point (the lattice join? or is it meet?) whatever, 
-- the greatest point
-- BTW, in the name of consLub, was Lub correct? should it have been
-- consGlb (greater lower bound)?
-- Then what we search for in here is lub!
-- The addition of 'qux' could be made generic -- it is the opposite
-- of narrow. But we don't care of genericity at this point...
list'of'foos self =
	   [\x -> (fooClass x self) >>= 
	           (\r -> return ( (qux .=. undefined) .*. r)),
	    \x -> fooClass2 x x self]

-- instead of passing the class constructor, we pass an index...
client2' n =
       do
       putStrLn "client2'"
       o1 <- mfix (\self -> ((list'of'foos self) !! n) True)
       o1 # bar
       o2 <- mfix (\self -> ((list'of'foos self) !! n) "True")
       o2 # bar



main = do 
          client1 family1
          client2 fooClass
	  client1'
	  client2' 0
	  client2' 1
