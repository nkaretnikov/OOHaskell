all:

test: 
	ghci -v0 nonextensible.hs < Main.in > nonextensible.out
	diff nonextensible.out nonextensible.ref
	ghci -v0 extensible.hs < Main.in > extensible.out
	diff extensible.out extensible.ref
	g++ reference.cpp -o reference
	./reference > reference.out
	diff reference.out reference.ref

clean:
	rm -f *~
	rm -f *.out
	rm -f reference
