
nst-manual.md:
	mlisp -e "(require 'asdf)" \
		-e '(push #p"../" asdf:*central-registry*)' \
		-e '(push #p"../../Lib/Lisp/closer-mop/" asdf:*central-registry*)' \
		-e '(push #p"../../Lib/Lisp/iterate/" asdf:*central-registry*)' \
		-e '(push #p"../../Lib/Lisp/org-sampler/" asdf:*central-registry*)' \
		-e '(asdf:load-system :nst)' \
		-e '(nst-asd::nst-doc)' \
		-kill
	emacs nst-manual.org --eval '(org-gfm-export-to-markdown)' \
		             --eval '(save-buffers-kill-terminal)'

clean: 
	rm nst-manual.md
