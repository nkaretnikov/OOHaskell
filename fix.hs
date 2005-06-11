-- The normal fix-point operator
fix f = let x = f x in x

-- Arg type is part of result type
foo :: s -> [s]
foo = undefined

{-
*Main> fix foo
<interactive>:1:4:
    Occurs check: cannot construct the infinite type: t = [t]
      Expected type: t -> t
      Inferred type: t -> [t]
    In the first argument of `fix', namely `foo'
    In the definition of `it': it = fix foo
-}

bar :: s -> [Int]
bar = undefined

{-
*Main> fix bar
*** Exception: Prelude.undefined
-}
