{-# OPTIONS -fglasgow-exts #-}

{-

(C) 2004-2005, Oleg Kiselyov, Ralf Laemmel

[DISCLAIMER: this file lives in the OOHaskell source distribution,
but it does not use OOHaskell. We have used the list examples in
various presentations throughout 2004; Dagstuhl on DTP; MSR 
presentation on type-level computations and others.]

We show that Haskell's type classes combined with an open style of
data type declaration can be effectively used to provide polymorphic
variants to Haskell (think of them as potentially polymorphic and
extensible data types).

To do so, we illustrate the extensible list example
from this standard reference:

@misc{ garrigue00code,
  author = "J. Garrigue",
  title = "Code reuse through polymorphic variants",
  text = "J. Garrigue. Code reuse through polymorphic variants.
          In Workshop on Foundations of Software Engineering,
          Sasaguri, Japan, November 2000.",
  year = "2000",
  url = "citeseer.ist.psu.edu/garrigue00code.html" 
}

The use of multi-parameter classes is indeed necessary for polymorphic
extensible datatypes. For monomorphic datatypes, Haskell 98's type
classes are sufficient.

In the above-mentioned paper, Garrigue also gives an extensible lambda
evaluator.  A similar example can be found here: a simple language
interpreter.

http://homepages.cwi.nl/~ralf/OOHaskell/src/interpreter/nonextensible.hs
http://homepages.cwi.nl/~ralf/OOHaskell/src/interpreter/extensible.hs

Discussion:

TO BE WORKED OUT ACCORDING TO AND BEYOND THAT:

- http://compilers.iecc.com/comparch/article/04-12-111
- http://www.mail-archive.com/haskell@haskell.org/msg16090.html

1. Instance constraints

This is additional effort compared to proper polymorphic variants.
However, the type checker "knows" all these constraints anyhow.  An
argument similar to (in fact much simpler than) woobly types could be
made to stop requesting the explicit declaration of the constraints. 
This could be naturally combined with trivial syntax sugar.

TO BE WORKED ON.

-}

import Prelude hiding(length)

-- --------------------------------------------------------------------------

-- Garrigue, OCaml
-- type 'a list = Nil | Cons of 'a * 'a list

data Nil a = Nil
data List x a => Cons x a = Cons a (x a)

class List (x :: * -> *) a
instance List Nil a
instance List x a => List (Cons x) a


-- --------------------------------------------------------------------------

-- Garrigue, OCaml
-- val length : 'a list -> int

class List x a => Length x a
 where
   length :: x a -> Int


-- --------------------------------------------------------------------------

{-

-- Garrigue, OCaml

let rec length = function 
   Nil          -> 0
 | Cons(hd, tl) -> 1 + length tl

-}

instance Length Nil a
  where length Nil = 0

instance Length x a => Length (Cons x) a
  where length (Cons hd tl) = 1 + length tl

 
-- --------------------------------------------------------------------------

-- Garrigue, OCaml
-- type 'a list = ... | Conc of 'a list 'a list

data (List x a, List y a) => Conc x y a = Conc (x a) (y a)

instance (List x a, List y a) => List (Conc x y) a


-- --------------------------------------------------------------------------

{-

-- Garrigue, OCaml

let rec length = function 
    ...
  | Conc(l1,l2) -> length l1 + length l2

-}

instance (Length x a, Length y a) => Length (Conc x y) a
  where length (Conc l1 l2) = length l1 + length l2

-- --------------------------------------------------------------------------

term1 = Cons 1 (Cons 2 Nil)
test1 = length term1

main = do 
          print test1

