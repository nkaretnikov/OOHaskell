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

%.ghci:
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

test: OCamlTutorial.test test-shapes test-many

##############################################################################

# Test the shapes examples only

test-shapes:

# The introductory shapes example
	$(MAKE) Shapes.test

# Variations on the shapes example (using the same baseline)
	$(MAKE) ShapesNarrow.shapes-test
	$(MAKE) ShapesHList.shapes-test
	$(MAKE) ShapesExists.shapes-test

# Even more shapes examples
	$(MAKE) ShapesEither.test
	$(MAKE) ShapesEitherR.test
	$(MAKE) ShapesGlb.test

##############################################################################

# Test many more OOHaskell examples

test-many: MultipleInheritance.test OoCopy.test \
	SimpleST.test   \
	CircBuffer.test \
	Selfish.test \
	SelfishSafe.test \
	SelfReturn.test \
	DynamicOo.test \
	DeepSubtyping.test \
	CovariantReturn.test \
	CovariantArgs.test \
	RecList.test     \
	NominalTest.test \
	EiffelFaqLcon.test

##############################################################################

clean:
	rm -f *.out

##############################################################################
