
all: check.ps classes.ps fixtures.ps format.ps interactive.ps manual.ps \
	nst.ps nst-nst.ps numbers.ps package.ps permuter.ps runners.ps  \
	test.ps test2.ps testforms.ps
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
