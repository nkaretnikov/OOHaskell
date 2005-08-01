ghci = ghci \
		-fglasgow-exts \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-i./HList

hs =	OOHaskell.hs	   \
	SimpleIO.hs	   \
	SimpleST.hs	   \
	CircBuffer.hs	   \
	Selfish.hs	   \
	Shapes.hs	   \
	SelfReturn.hs	   \
	CovariantReturn.hs \
	RecList.hs 	   \
	Covariance.hs	   \
	LocalSigs.hs	   \
	TwoTables.hs

##############################################################################
#
# By default build source distribution
#

all: index.html OOHaskell.zip

##############################################################################
#
# Use this for starting a ghci session
#

%.ghci: HList
	${ghci}	$*.hs


##############################################################################
#
# Precompilation
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


index.html: pre.html README post.html
	cat pre.html README post.html > index.html

OOHaskell.zip: *.hs *.ref *.html Makefile README
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
	${ghci}	-v0 DownCast.hs < Main.in > DownCast.out
	diff -b DownCast.out DownCast.ref
	${ghci}	-v0 CovariantReturn.hs < Main.in > CovariantReturn.out
	diff -b CovariantReturn.out CovariantReturn.ref
	${ghci}	-v0 RecList.hs < Main.in > RecList.out
	diff -b RecList.out RecList.ref
	${ghci}	-v0 Covariance.hs < Main.in > Covariance.out
	diff -b Covariance.out Covariance.ref
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

test-all: test
	(cd Rathman; make test)
	(cd interpreter; make test)



##############################################################################
#
# Remind the user of the need to link to HList library
#

HList:
	@echo
	@echo "*****************************************"
	@echo "* Link the HList src library to ./HList *"
	@echo "*****************************************"
	@echo


##############################################################################
#
# Start a ghci session for selfish examples
#

self:
	${ghci} \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-i./HList Selfish.hs

##############################################################################
#
# Clean up file system
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
