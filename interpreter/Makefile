all:

test: 
	ghci -v0 nonextensible.hs < Main.in > nonextensible.out
	diff -b nonextensible.out nonextensible.ref
	ghci -v0 extensible.hs < Main.in > extensible.out
	diff -b extensible.out extensible.ref
	g++ reference.cpp -o reference
	./reference > reference.out
	diff -b reference.out reference.ref

clean:
	rm -f *~
	rm -f *.out
	rm -f *.exe
	rm -f reference
