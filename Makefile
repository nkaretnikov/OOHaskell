ghci = ghci -i./HList

simple:
	${ghci}	$@.hs

open-rec:
	${ghci}	$@.hs

clean:
	rm -f *~
