ghci = ghci -i./HList
#GHC > 6.2 does not work because of API changes for Typeable/Data.
#ghci = /home/ralf/cvs/software/ghc-fptools/ghc/compiler/stage2/ghc-inplace --interactive -i./HList

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
	cp --preserve *OO.hs *.in *.ref README Makefile OOHaskell
	(cd interpreter; gmake clean)
	(cd Rathman; gmake clean)
	(cd PoorMens; gmake clean)
	cp --preserve -r interpreter Rathman PoorMens OOHaskell
	rm -rf OOHaskell/interpreter/CVS
	rm -rf OOHaskell/Rathman/CVS
	rm -rf OOHaskell/PoorMens/CVS
	zip -r OOHaskell.zip OOHaskell
	rm -rf OOHaskell

##############################################################################
#
# Run test cases
#
test: HList
	${ghci}	-v0 SimpleOO.hs < Main.in > SimpleOO.out
	diff SimpleOO.out SimpleOO.ref
	${ghci}	-v0 SelfOO.hs < Main.in > SelfOO.out
	diff SelfOO.out SelfOO.ref
	${ghci}	-v0 ShapesOO.hs < Main.in > ShapesOO.out
	diff ShapesOO.out ShapesOO.ref
	(cd interpreter; gmake test)
	(cd Rathman; gmake test)
	(cd PoorMens; gmake test)

##############################################################################
#
# Approve generated output as test results
#
copy:
	cp SimpleOO.out SimpleOO.ref
	cp SelfOO.out   SelfOO.ref
	cp ShapesOO.out ShapesOO.ref

##############################################################################
#
# Remind the use of the need to link to HList library
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
	rm -f index.html OOHaskell.zip
	(cd interpreter; gmake clean)
	(cd Rathman; gmake clean)
	(cd PoorMens; gmake clean)
