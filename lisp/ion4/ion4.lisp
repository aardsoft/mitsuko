(defpackage :mitsuko-compositor
  (:use :cl :eql :mitsuko-core)
  (:export
   #:init-module))

(in-package :mitsuko-compositor)

(defun init-module()
  ""
  (format t "ion4 compositor starting up.~%")
  )
