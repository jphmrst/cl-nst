
all: package.ps nst.ps test.ps check.ps \
	interactive.ps numbers.ps permuter.ps \
	nst-nst.ps format.ps testforms.ps fixtures.ps \
	classes.ps runners.ps
	#

%.ps: %.lisp
	enscript -2r --lines-per-page=57 -o $@ $<

clean:
	rm -f *.fasl *.ps
