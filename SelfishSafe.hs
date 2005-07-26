{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-undecidable-instances #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

OOHaskell (C) 2004, Oleg Kiselyov, Ralf Laemmel, Keean Schupke

This module illustrates the notion of self, i.e., open recursion. 
Safety

-}


module SelfishSafe where

import OOHaskell
import qualified Prelude (print)
import Prelude hiding (print)
import SMRFix
import Debug.Trace

infixr 9 #
m # field = (m .!. field)


-- First, declare the labels.
-- We use proxies as of HList/Label4.hs

data MutableX; mutableX = proxy::Proxy MutableX
data GetX;     getX     = proxy::Proxy GetX
data Move;     move     = proxy::Proxy Move
data Print;    print    = proxy::Proxy Print


printable_point x_init s =
   do
      x <- newIORef x_init
      -- If we uncomment this, we get the type error.
      -- Alas, the error is reported at the place of smfix rather than here...
      -- s # print 
      srret s (\s->
	   --trace "in srret" $
	   -- seq (trace "in srret, seq" 1) $
	   -- s `seq`
	   -- seq (trace "in srret, seq 2" 1) $
           mutableX .=. x
       .*. getX     .=. readIORef x
       .*. move     .=. (\d -> modifyIORef x ((+) d))
       .*. print    .=. ((s # getX ) >>= Prelude.print)
       .*. emptyRecord)

test_pp =
   do
      p <- smrfix (printable_point 7)
      p # move $ 2
      p # print

-- We need another label.
data GetColor; getColor = proxy::Proxy GetColor

-- Inheritance is simple: just adding methods ...
colored_point x_init (color::String) self =
   do
        p <- printable_point x_init self
	-- This causes the type error (alas, at the confusing place)
	-- p # print
        srret p $ \p -> getColor .=. (returnIO color) .*. p


myColoredOOP =
   do
      p' <- smrfix (colored_point 5 "red")
      x  <- p' # getX
      c  <- p' # getColor
      Prelude.print (x,c)

-- We derive a better class of colored points, which prints more accurately.
-- To this end, we access the overriden method akin to the OCaml super.

colored_point' x_init color self =
   do
      p <- colored_point x_init color self
      srret p $ \p -> 
	  -- Here, it's OK to access the method print of p,
	  -- even if p isn't constructed yet. Courtesy of non-strict
	  -- evaluation, old_print will be evaluated only when the construction
	  -- finishes.
	  let old_print = p # print in
	  print .=. (
              do putStr "so far - "; old_print
                 putStr "color  - "; Prelude.print color )
            .<. p

myOverridingOOP =
   do
      p  <- smrfix (colored_point' 5 "red")
      p  # print



testGeneric
   = do
        p  <- smrfix (printable_point 7)
        p' <- smrfix (colored_point 5 "red")
        let get_succ_x obj = obj # getX >>= (returnIO . (+ 1))
        x  <- get_succ_x p
        x' <- get_succ_x p'
        Prelude.print $ x+x'


-- Note, compared with printable_point, we omitted the virtual methods.
-- That made abstract_point uninstantiatable!!!

-- This is an optional part in case we want to fix types of virtuals.
{-

abstract_point (x_init::a) (self :: NotConstructed r)
  | const False (constrain (undefined::r) ::
                 Proxy ( (Proxy GetX, IO a)
                      :*: (Proxy Move, a -> IO ())
                      :*: HNil ))
  = undefined
-}
abstract_point x_init self =
   do
      x <- newIORef x_init
      srret self $ \self ->
           mutableX  .=. x
       .*. print     .=. (self # getX >>= Prelude.print )
       .*. emptyRecord


concrete_point x_init self
   = do
        p <- abstract_point x_init self -- inherit ...
        srret (p,self) $ \ (p,self) ->
        -- add the missing (pure virtual) methods
             getX  .=. readIORef (self # mutableX)
         .*. move .=. (\d -> modifyIORef (self # mutableX) ((+) d))
         .*. p

testVirtual
   = do
        p  <- smrfix (concrete_point 7)
        --
        -- Note, if the latter is uncommented
        --   p' <- smrfix (abstract_point 7)
        -- we see an error that means "field getX missing"
        -- which reads as follows:
        -- (HasField (Proxy GetX) HNil (IO a))
        p # getX >>= Prelude.print
        p # move $ 2
        p # getX >>= Prelude.print
        p # print

-- This abstract point class mentions the type of the virtual methods.

abstract_point' x_init self
  = do
      x <- newIORef x_init
      srret self $ \self ->
	   mutableX  .=. x
       .*. getX      .=. (proxy::Proxy (IO Int))
       .*. move      .=. (proxy::Proxy (Int -> IO ()))
       .*. print     .=. (self # getX >>= Prelude.print )
       .*. emptyRecord


-- Another label for testing purposes
data MyLabel; myLabel = proxy::Proxy MyLabel


-- This concrete class implements all virtual methods
concrete_point' x_init self
   = do
        p <- abstract_point' x_init self -- inherit ...
        srret (p,self) $ \(p,self) ->
        -- use disciplined record update
              getX    .=. readIORef (self # mutableX)
          .^. move   .=. (\d -> modifyIORef (self # mutableX) ((+) d))
          .^. myLabel .=. ()                -- This line could be activated.
--        .^. myLabel .=. (proxy::Proxy ()) -- A proxy that disables mnew.
          .*. p

-- We introduce a constrained new method to refuse proxy fields in records.
mnew (f::NotConstructed a -> m (NotConstructed a)) = smrfix f
 where
  () = hasNoProxies (undefined::a) 

testVirtual'
   = do
        p <- mnew (concrete_point' 7)
        p # getX >>= Prelude.print
        p # move $ 2
        p # getX >>= Prelude.print
        p # print


main =
  do 
     putStrLn "Simple Printable Pt"     ; test_pp
     putStrLn "myColoredOOP"     ; myColoredOOP
     putStrLn "myOverridingOOP"  ; myOverridingOOP
     putStrLn "testGeneric"      ; testGeneric
     putStrLn "testVirtual"      ; testVirtual
     putStrLn "testVirtual'"     ; testVirtual'


-- :t colored_point
-- :t mfix $ colored_point (1::Int) "red"

