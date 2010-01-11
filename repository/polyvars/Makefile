all:

test: 
	ghci -v0 list1.hs < Main.in > list1.out
	diff -b list1.out list1.ref

clean:
	rm -f *~
	rm -f *.out
