all:

test: 
	g++ shapes.cc
	./a.out > shapes.out
	diff shapes.out shapes.ref

clean:
	rm -f *~
	rm -f a.out
