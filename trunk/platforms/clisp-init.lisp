(setf asdf:*central-registry*
      (list* #P"/home/jm/Lib/Lisp/asdf-install/asdf-install/"
             #P"/home/jm/.asdf-install-dir/systems/"
             asdf:*central-registry*))
(asdf:oos 'asdf:load-op :asdf-binary-locations)
(setf asdf:*centralize-lisp-binaries* t
      asdf:*default-toplevel-directory* #P"/home/jm/Lib/Lisp/fasl/clisp/")

