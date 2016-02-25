
README.md:
	(cd doc; make nst-manual.md)
	cp doc/nst-manual.md README.md

clean:
	rm -rf doc/html
	rm -f *.ps */*.ps */*/*.ps \
	      *.fasl */*.fasl */*/*.fasl */*/*/*.fasl \
	      *~ */*~ */*/*~ \
	      doc/gen/*.tex doc/manual.ps doc/manual.pdf \
	      doc/*.dvi doc/*.aux doc/*.log doc/*.toc \
	      doc/*.idx doc/*.out doc/*.pdf doc/*.ilg doc/*.ind \
	      doc/*/*.dvi doc/*/*.aux doc/*/*.log doc/*/*.toc \
	      doc/*/*.idx doc/*/*.out doc/*/*.pdf doc/*/*.ilg doc/*/*.ind
