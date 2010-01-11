# Pointer to GHC

ghci = ghci -i../../HList/:../lib/:.

# A list of the samples

hs =			    \
	OOHaskell.hs	    \
	DeepNarrow.hs	    \
	SimpleIO.hs	    \
	SimpleST.hs	    \
	CircBuffer.hs	    \
	Selfish.hs	    \
	SelfishSafe.hs	    \
	Shapes*.hs	    \
	SelfReturn.hs	    \
	RecList.hs 	    \
	New.hs	 	    \
	Nominal.hs 	    \
	NominalTest.hs 	    \
	DeepSubtyping.hs    \
	CovariantReturn.hs  \
	CovariantArgs.hs    \
	EiffelFaqLcon.hs    \
	DynamicOo.hs

##############################################################################

# Nothing to be done.

all:
	@echo "make test?"

##############################################################################

# Use this for starting a ghci session for a specific sample

%.ghci: HList
	${ghci}	$*.hs

##############################################################################

# Test a particular OOHaskell sample (assuming a baseline of the same name)

%.test:
	@rm -f $*.out
	@${ghci} -v0 $*.hs < ../include/Main.in > $*.out
	@diff -b $*.out refs/$*.ref
	@rm -f $.*out

##############################################################################

# Like %.test, but for a group of shapes examples with the same baseline

%.shapes-test:
	@rm -f $*.out
	@${ghci} -v0 $*.hs < ../include/Main.in > $*.out
	@diff -b $*.out refs/Shapes.ref
	@rm -f $.*out

##############################################################################

# Test all the OOHaskell examples

test:
	make OCamlTutorial.test
	make test-shapes
	make test-many

##############################################################################

# Test the shapes examples only

test-shapes:

# The introductory shapes example
	make Shapes.test

# Variations on the shapes example (using the same baseline)
	make ShapesNarrow.shapes-test
	make ShapesHList.shapes-test
	make ShapesExists.shapes-test

# Even more shapes examples
	make ShapesEither.test
	make ShapesEitherR.test
	make ShapesGlb.test

##############################################################################

# Test many more OOHaskell examples

test-many:
	make MultipleInheritance.test
	make OoCopy.test
	make SimpleST.test
	make CircBuffer.test
	make Selfish.test
	make SelfishSafe.test
	make SelfReturn.test
	make DynamicOo.test
	make DeepSubtyping.test
	make CovariantReturn.test
	make CovariantArgs.test
	make RecList.test
	make NominalTest.test
	make EiffelFaqLcon.test

##############################################################################

clean:
	rm -f *.out

##############################################################################
