##############################################################################
#
# Useful make targets
#
# make test        -- run all (GHC-based) test cases.
# make clean       -- remove all generated and temporary and backup files
# See more below!  -- Not all targets are well maintained.
#


##############################################################################
#
# Some variables
#

# Pointer to GHC
ghci = ghci -i../HList/:lib/:samples/

ghci-old = ghci \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-i../HList/

# All the local samples
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
#
# By default tell user to have a look at the Makefile's header.
#

all:
	@echo
	@echo "*****************************************************"
	@echo "* See the Makefile's header for reasonable targets. *"
	@echo "* Perhaps, you may want to run make test?           *"
	@echo "*****************************************************"
	@echo


##############################################################################
#
# Use this for starting a ghci session
#

%.ghci: HList
	${ghci}	$*.hs



##############################################################################
#
# Run test cases
#  Use "make test" for GHC-only test cases.
#  Use "make test-all" to include C/C++/Hugs test cases.
#

# Start a ghci session for a particular OOHaskell module

%.ghci:
	${ghci}	-v0 $*.hs


# Test a particular OOHaskell sample (assuming a baseline of the same name)

%.test:
	@rm -f samples/$*.out
	@${ghci} -v0 samples/$*.hs < include/Main.in > samples/$*.out
	@diff -b samples/$*.out samples/refs/$*.ref
	@rm -f samples/$.*out


# Like %.test but for a group of shapes examples with the same baseline

%.shapes-test:
	@rm -f samples/$*.out
	@${ghci} -v0 samples/$*.hs < include/Main.in > samples/$*.out
	@diff -b samples/$*.out samples/refs/Shapes.ref
	@rm -f samples/$.*out


# Test all the OOHaskell examples

test:
	make OCamlTutorial.test
	make test-shapes
	make test-many


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


# Test also some object encodings not using OOHaskell

test-NonOOHaskell:
	(cd Shapes1; make test)
	(cd Shapes2; make test)
	(cd Shapes3; make test)
	(cd Shapes4; make test)
	(cd Shapes5; make test)
	(cd Shapes6; make test)
	(cd Shapes7; make test)
	(cd Shapes8; make test)

##############################################################################
#
# Clean up things
#

clean:
	rm -f samples/*.out


##############################################################################
#
# Precompilation of OOHaskell.
# This target has not been maintained in a while.
#

OOHaskell.o: OOHaskell.hs HList/*.hs Makefile
	rm -f HList/*.o
	ghc  \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		--make \
		-i./HList \
		OOHaskell.hs


##############################################################################
#
# Target used by the authors for distributing OOHaskell.
# This target has not been maintained in a while.
#

distr:
	cat pre.html README post.html > index.html
	rm -f OOHaskell.zip
	(cd HList; make distr)
	rm -rf OOHaskell
	mkdir -p OOHaskell
	cp --preserve ${hs} *.in *.ref README LICENSE Makefile OOHaskell
	(cd Weirich; make clean)
	(cd Rathman; make clean)
	(cd Shapes1; make clean)
	(cd Shapes2; make clean)
	(cd Shapes3; make clean)
	(cd Shapes4; make clean)
	(cd Shapes5; make clean)
	(cd Shapes6; make clean)
	(cd interpreter; make clean)
	cp --preserve HList/HList.zip OOHaskell
	(cd OOHaskell; unzip HList.zip; rm HList.zip)
	cp --preserve -r Weirich Rathman Shapes1 Shapes2 Shapes3 Shapes4 Shapes5 Shapes6 interpreter OOHaskell
	rm -rf OOHaskell/Weirich/CVS
	rm -rf OOHaskell/Rathman/CVS
	rm -rf OOHaskell/Shapes1/CVS
	rm -rf OOHaskell/Shapes2/CVS
	rm -rf OOHaskell/Shapes3/CVS
	rm -rf OOHaskell/Shapes4/CVS
	rm -rf OOHaskell/Shapes5/CVS
	rm -rf OOHaskell/Shapes6/CVS
	rm -rf OOHaskell/interpreter/CVS
	zip -r OOHaskell.zip OOHaskell


##############################################################################
#
# Remind the CVS user of the need to link to the HList library
# A user of OOHaskell should not see this message.
# These days HList is distributed with OOHaskell.
#

HList:
	@echo
	@echo "*****************************************"
	@echo "* Link the HList src library to ./HList *"
	@echo "*****************************************"
	@echo


##############################################################################

commit:
	darcs record -a -m "Committed from the Makefile"
	darcs push -a
