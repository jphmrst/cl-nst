
all: classes.ps globals.ps nst-nst.ps scratch.ps criteria.ps \
	interactive.ps numbers.ps status.ps defcheck.ps \
	nst-criteria.ps package.ps test2.ps fixtures.ps nst-fails.ps \
	permuter.ps testforms.ps format.ps nst-interact.ps runners.ps \
	test.ps \
	manual.pdf
	#

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

%.dvi: %.tex
	latex $<
	makeindex $*
	latex $<

%.ps: %.dvi
	dvips -f $< > $@

%.ps: %.lisp
	enscript -2r --lines-per-page=57 -o $@ $<

clean:
	rm -f *.fasl *.ps *.dvi *.aux *.log *.toc
