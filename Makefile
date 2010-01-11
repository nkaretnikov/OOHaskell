all:
	@echo "make test?"

test:
	cd samples; make test

clean:
	cd samples; make clean

commit:
	darcs record -a -m "Committed from the Makefile"
	darcs push -a
