ghci = ghci -i./HList
#GHC > 6.2 does not work because of API changes for Typeable/Data.
#ghci = /home/ralf/cvs/software/ghc-fptools/ghc/compiler/stage2/ghc-inplace --interactive -i./HList
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

index.html: pre.html README post.html
	cat pre.html README post.html > index.html

OOHaskell.zip: *.hs *.ref *.html Makefile README
	mkdir -p OOHaskell
	cp --preserve ${hs} *.in *.ref README LICENSE Makefile OOHaskell
	(cd Weirich; make clean)
	(cd Rathman; make clean)
	(cd PoorMens0; make clean)
	(cd PoorMens1; make clean)
	(cd PoorMens2; make clean)
	(cd PoorMens3; make clean)
	(cd interpreter; make clean)
	cp --preserve -r Weirich Rathman PoorMens0 PoorMens1 PoorMens2 PoorMens3 interpreter OOHaskell
	rm -rf OOHaskell/Weirich/CVS
	rm -rf OOHaskell/Rathman/CVS
	rm -rf OOHaskell/PoorMens0/CVS
	rm -rf OOHaskell/PoorMens1/CVS
	rm -rf OOHaskell/PoorMens2/CVS
	rm -rf OOHaskell/PoorMens3/CVS
	rm -rf OOHaskell/interpreter/CVS
	zip -r OOHaskell.zip OOHaskell

##############################################################################
#
# Run test cases
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
	${ghci}	-v0 Shapes.hs < Main.in > Shapes.out
	diff -b Shapes.out Shapes.ref
	${ghci}	-v0 SelfReturn.hs < Main.in > SelfReturn.out
	diff -b SelfReturn.out SelfReturn.ref
	${ghci}	-v0 CovariantReturn.hs < Main.in > CovariantReturn.out
	diff -b SelfReturn.out SelfReturn.ref
	${ghci}	-v0 RecList.hs < Main.in > RecList.out
	diff -b RecList.out RecList.ref
	${ghci}	-v0 Covariance.hs < Main.in > Covariance.out
	diff -b Covariance.out Covariance.ref
	${ghci}	-v0 LocalSigs.hs < Main.in > LocalSigs.out
	diff -b LocalSigs.out LocalSigs.ref
	${ghci}	-v0 TwoTables.hs < Main.in > TwoTables.out
	diff -b TwoTables.out TwoTables.ref
	(cd interpreter; make test)
	(cd Rathman; make test)
	(cd PoorMens0; make test)
	(cd PoorMens1; make test)
	(cd PoorMens2; make test)
	(cd PoorMens3; make test)


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
# Start a ghci session for the shapes benchmark
#
shapes:
	${ghci} \
		-fallow-overlapping-instances \
		-fallow-undecidable-instances \
		-i./HList Shapes.hs

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
	rm -f index.html OOHaskell.zip
	(cd Weirich; make clean)
	(cd Rathman; make clean)
	(cd PoorMens0; make clean)
	(cd PoorMens1; make clean)
	(cd PoorMens2; make clean)
	(cd PoorMens3; make clean)
	(cd interpreter; make clean)
