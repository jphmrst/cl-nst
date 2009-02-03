
all: anon-fixtures-mnst.ps \
	byhand-mnst.ps \
	check.ps \
	criteria.ps \
	fixture.ps \
	globals.ps \
	group.ps \
	interactive.ps \
	package.ps \
	permuter.ps \
	simple-mnst.ps \
	status.ps \
	test.ps
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
