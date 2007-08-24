all:

build:
	javac *.java

check: build
	java Polymorph > Polymorph.out
	diff Polymorph.out Polymorph.ref

clean:
	rm -f *~
	rm -f *.class
	rm -f *.out
