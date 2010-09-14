
docs:
	(cd doc/manual; ./make-generated.lisp; pdflatex manual; makeindex manual; pdflatex manual)
	(cd doc/quickref; pdflatex quickref; pdflatex quickref)
	@echo
	@echo NST documentation generated
	@echo - User manual in doc/manual/manual.pdf
	@echo - Quick reference card in doc/quickref/quickref.pdf

clean:
	rm -f *.fasl *.ps */*.ps */*/*.ps \
	      *~ */*~ */*/*~ \
	      doc/*/*.dvi doc/*/*.aux doc/*/*.log doc/*/*.toc \
	      doc/*/*.idx doc/*/*.out doc/*/*.pdf doc/*/*.ilg doc/*/*.ind
