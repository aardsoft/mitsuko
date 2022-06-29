(defpackage :mitsuko-compositor
  (:use :cl :eql :mitsuko-core)
  (:export
   #:init-module
   #:post-init-module))

(in-package :mitsuko-compositor)

(defun init-module()
  ""
  (format t "float compositor starting up.~%")
  )

(defun post-init-module()
  ""
  (format t "float compositor started up.~%")
  )
