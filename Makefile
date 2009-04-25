
all: asdf/package.ps \
	asdf/system.ps \
	core/check.ps \
	core/command.ps \
	core/criteria.ps \
	core/fixture.ps \
	core/globals.ps \
	core/group.ps \
	core/package.ps \
	core/permuter.ps \
	core/status.ps \
	interactive.ps \
	self-test/asdf/tests.ps \
	self-test/core/anon-fixtures-mnst.ps \
	self-test/core/byhand-mnst.ps \
	self-test/core/simple-mnst.ps \
	self-test/core/test.ps
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
	rm -f *.fasl \
      doc/*/*.ps  doc/*/*.dvi doc/*/*.aux doc/*/*.log doc/*/*.toc \
      doc/*/*.idx doc/*/*.out doc/*/*.pdf doc/*/*.ilg doc/*/*.ind
