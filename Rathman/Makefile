all:

test: 
	ghci -v0 Polymorph.hs < Main.in > Polymorph.out
	diff Polymorph.out Polymorph.ref

clean:
	rm -f *~
