
all: package.ps \
	globals.ps group.ps check.ps fixture.ps \
	permuter.ps criteria.ps status.ps \
	mnst-package.ps \
	sample.ps \
	interactive.ps test.ps \

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
