#-eql5
(error "Please use the EQL5 executable (see README)")

(require :cmp)

;;(load "tr")

#+msvc
(setf c::*compile-in-constants* t)

(defparameter *lisp-files*
  (list "mitsuko-core"
        "qml-file")
  "All Lisp files of the application.")

(dolist (file *lisp-files*)
  (load file)
  (compile-file file :system-p t))

(c:build-static-library "mitsuko_lib"
                        :lisp-files (mapcar (lambda (file)
                                              (x:cc file #+msvc ".obj"
                                                    #-msvc ".o"))
                                            *lisp-files*)
                        ;; name as computed in ASDF version
                        :init-name "init_lib_MITSUKO__ALL_SYSTEMS")
(eql:qq)
