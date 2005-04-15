all:

test: 
	ghci -v0 Polymorph.hs < Main.in > Polymorph.out
	diff -b Polymorph.out Polymorph.ref

clean:
	rm -f *~
