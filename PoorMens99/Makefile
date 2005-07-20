all:

test: 
	ghci -v0 Polymorph.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Existential.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Inference.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 -i../HList Heterogeneous.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref

clean:
	rm -f *~
