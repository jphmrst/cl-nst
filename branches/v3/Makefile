
all: check.ps globals.ps mnst-package.ps sample.ps criteria.ps group.ps      \
	package.ps status.ps fixture.ps interactive.ps permuter.ps test.ps   \
	#manual.pdf
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
	a2ps -2r -o $@ $<

clean:
	rm -f *.fasl *.ps *.dvi *.aux *.log *.toc
