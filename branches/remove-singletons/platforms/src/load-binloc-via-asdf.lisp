(setf asdf:*central-registry*
      (list* #P"/home/jm/Lib/Lisp/asdf-install/asdf-install/"
             #P"/home/jm/.asdf-install-dir/systems/"
             asdf:*central-registry*))
(asdf:oos 'asdf:load-op :asdf-binary-locations)
