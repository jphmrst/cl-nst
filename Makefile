
all: package.ps nst.ps test.ps check.ps \
		interactive.ps numbers.ps permuter.ps \
		nst-nst.ps format.ps testforms.ps fixtures.ps \
		classes.ps runners.ps manual.ps
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
