(load "/usr/share/common-lisp/source/cl-asdf/asdf")

(setf asdf:*central-registry*
      (list* #P"/home/jm/Lib/Lisp/asdf-install/asdf-install/"
             #P"/home/jm/.asdf-install-dir/systems/"
             asdf:*central-registry*))
(asdf:oos 'asdf:load-op :asdf-binary-locations)

(let ((asdf:*central-registry*
       (list #P"/home/jm/Lib/Lisp/SIFT/asd-finder/"
             #P"/home/jm/Lib/Lisp/SIFT/asdf-context/")))
  (declare (special asdf:*central-registry*))
  (asdf:oos 'asdf:load-op :asd-finder)
  (asdf:oos 'asdf:load-op :asdf-context))
