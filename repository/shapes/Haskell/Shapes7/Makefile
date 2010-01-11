all:

test: 
	ghci -v0 Polymorph.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Implicit1.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Implicit2.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Existential.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Existential1.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
	ghci -v0 Heterogeneous.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref
#	ghci -v0 Inference.hs < Main.in > Polymorph.out
#	diff -b Polymorph.out Polymorph.ref
#	ghci -v0 -i../../HList/src:../../HList/examples Heterogeneous.hs < Main.in > Polymorph.out
#	diff -b Polymorph.out Polymorph.ref

clean:
	rm -f *~
