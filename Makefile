ghci = ghci -i./HList
#GHC > 6.2 does not work because of API changes for Typeable/Data.
#ghci = /home/ralf/cvs/software/ghc-fptools/ghc/compiler/stage2/ghc-inplace --interactive -i./HList
hs =	OOHaskell.hs	\
	SimpleIO.hs	\
	SimpleST.hs	\
	CircBuffer.hs	\
	Selfish.hs	\
	Shapes.hs	\
	SelfReturn.hs	\
	Covariance.hs	\
	LocalSigs.hs	\
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
	(cd Weirich; gmake clean)
	(cd Rathman; gmake clean)
	(cd PoorMens; gmake clean)
	(cd PoorMens2; gmake clean)
	(cd interpreter; gmake clean)
	cp --preserve -r Weirich Rathman PoorMens PoorMens2 interpreter OOHaskell
	rm -rf OOHaskell/Weirich/CVS
	rm -rf OOHaskell/Rathman/CVS
	rm -rf OOHaskell/PoorMens/CVS
	rm -rf OOHaskell/PoorMens2/CVS
	rm -rf OOHaskell/interpreter/CVS
	zip -r OOHaskell.zip OOHaskell

##############################################################################
#
# Run test cases
#
test: HList
	${ghci}	-v0 SimpleIO.hs < Main.in > SimpleIO.out
	diff SimpleIO.out SimpleIO.ref
	${ghci}	-v0 SimpleST.hs < Main.in > SimpleST.out
	diff SimpleST.out SimpleST.ref
	${ghci}	-v0 CircBuffer.hs < Main.in > CircBuffer.out
	diff CircBuffer.out CircBuffer.ref
	${ghci}	-v0 Selfish.hs < Main.in > Selfish.out
	diff Selfish.out Selfish.ref
	${ghci}	-v0 Shapes.hs < Main.in > Shapes.out
	diff Shapes.out Shapes.ref
	${ghci}	-v0 SelfReturn.hs < Main.in > SelfReturn.out
	diff SelfReturn.out SelfReturn.ref
	${ghci}	-v0 Covariance.hs < Main.in > Covariance.out
	diff Covariance.out Covariance.ref
	${ghci}	-v0 LocalSigs.hs < Main.in > LocalSigs.out
	diff LocalSigs.out LocalSigs.ref
	${ghci}	-v0 TwoTables.hs < Main.in > TwoTables.out
	diff TwoTables.out TwoTables.ref
	(cd interpreter; gmake test)
	(cd Rathman; gmake test)
	(cd PoorMens; gmake test)
	(cd PoorMens2; gmake test)


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
# Clean up file system
#
clean:
	rm -f *~
	rm -f *.out
	rm -f index.html OOHaskell.zip
	(cd Weirich; gmake clean)
	(cd Rathman; gmake clean)
	(cd PoorMens; gmake clean)
	(cd PoorMens2; gmake clean)
	(cd interpreter; gmake clean)
