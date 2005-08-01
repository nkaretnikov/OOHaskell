##############################################################################
#
# Useful make targets
#
# make test        -- run all GHC-based test cases.
# make test-all    -- run more test cases (C/C++/Hugs, if any) 
# make clean       -- remove all generated and temporary and backup files
# make OOHaskell.o -- precompile OOHaskell (use at your own risk!
#


##############################################################################
#
# Some variables
#

# Pointer to GHC
ghci = ghci \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-i./HList

# All the local samples
hs =			    \
	OOHaskell.hs	    \
	DeepNarrow.hs	    \
	SimpleIO.hs	    \
	SimpleST.hs	    \
	CircBuffer.hs	    \
	Selfish.hs	    \
	Shapes*.hs	    \
	SelfReturn.hs	    \
	RecList.hs 	    \
	DeepSubtyping.hs    \
	CovariantReturn.hs  \
	CovariantArgs.hs    \
	EiffelFaqLcon.h	    \
	DynamicOo.hs	    \
	LocalSigs.hs	    \
	TwoTables.hs


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

test: HList
	${ghci}	-v0 SimpleIO.hs < Main.in > SimpleIO.out
	diff -b SimpleIO.out SimpleIO.ref
	${ghci}	-v0 SimpleST.hs < Main.in > SimpleST.out
	diff -b SimpleST.out SimpleST.ref
	${ghci}	-v0 CircBuffer.hs < Main.in > CircBuffer.out
	diff -b CircBuffer.out CircBuffer.ref
	${ghci}	-v0 Selfish.hs < Main.in > Selfish.out
	diff -b Selfish.out Selfish.ref
	${ghci}	-v0 ShapesNarrow.hs < Main.in > Shapes.out
	diff -b Shapes.out Shapes.ref
	${ghci}	-v0 ShapesLub.hs < Main.in > Shapes.out
	diff -b Shapes.out Shapes.ref
	${ghci}	-v0 ShapesHList.hs < Main.in > Shapes.out
	diff -b Shapes.out Shapes.ref
	${ghci}	-v0 ShapesExists.hs < Main.in > Shapes.out
	diff -b Shapes.out Shapes.ref
	${ghci}	-v0 ShapesEither.hs < Main.in > Shapes.out
	diff -b Shapes.out ShapesDown.ref
	${ghci}	-v0 SelfReturn.hs < Main.in > SelfReturn.out
	diff -b SelfReturn.out SelfReturn.ref
	${ghci}	-v0 DynamicOo.hs < Main.in > DynamicOo.out
	diff -b DynamicOo.out DynamicOo.ref
	${ghci}	-v0 DeepSubtyping.hs < Main.in > DeepSubtyping.out
	diff -b DeepSubtyping.out DeepSubtyping.ref
	${ghci}	-v0 CovariantReturn.hs < Main.in > CovariantReturn.out
	diff -b CovariantReturn.out CovariantReturn.ref
	${ghci}	-v0 CovariantArgs.hs < Main.in > CovariantArgs.out
	diff -b CovariantArgs.out CovariantArgs.ref
	${ghci}	-v0 RecList.hs < Main.in > RecList.out
	diff -b RecList.out RecList.ref
	${ghci}	-v0 EiffelFaqLcon.hs < Main.in > EiffelFaqLcon.out
	diff -b EiffelFaqLcon.out EiffelFaqLcon.ref
	${ghci}	-v0 LocalSigs.hs < Main.in > LocalSigs.out
	diff -b LocalSigs.out LocalSigs.ref
	${ghci}	-v0 TwoTables.hs < Main.in > TwoTables.out
	diff -b TwoTables.out TwoTables.ref
	(cd Shapes1; make test)
	(cd Shapes2; make test)
	(cd Shapes3; make test)
	(cd Shapes4; make test)
	(cd Shapes5; make test)
	(cd Shapes6; make test)

#
# The following may require some particular C/C++ compilers.
# Don't mind if these tests don't complete fine.
# We also run the HList tests.
# The normal "test" target is enough for using OOHaskell with ghc(i).
#

test-all: test
	(cd Rathman; make test)
	(cd interpreter; make test)
	(cd HList; make test)


##############################################################################
#
# Clean up things
#

clean:
	rm -f *~
	rm -f *.out
	rm -f *.o
	rm -f *.hi
	(cd HList; make clean)
	rm -f index.html OOHaskell.zip
	(cd Weirich; make clean)
	(cd Rathman; make clean)
	(cd Shapes1; make clean)
	(cd Shapes2; make clean)
	(cd Shapes3; make clean)
	(cd Shapes4; make clean)
	(cd Shapes5; make clean)
	(cd Shapes6; make clean)
	(cd interpreter; make clean)


##############################################################################
#
# Precompilation of OOHaskell.
#
# BEWARE!!!
# This may not work even if interpretation works.
# Depending on versions and platforms.
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
