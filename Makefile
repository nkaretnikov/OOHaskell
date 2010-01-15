all:
	@echo "make test?"

test:
	cd samples; $(MAKE) test

clean:
	cd samples; $(MAKE) clean

commit:
	darcs record -a -m "See ChangeLog"
	darcs push -a
