(defpackage :mitsuko
  (:use :cl :eql :mitsuko-core)
  (:export
   #:*mitsuko-settings*
   #:load-settings-plugin
   ))

(in-package :mitsuko)

(defvar *mitsuko-settings* nil)

(defun load-settings-plugin ()
  ""
  (setf *mitsuko-settings* (qload-c++ "/home/bwachter/git/misc/mitsuko/build/mitsukosettings/libMitsukoSettings"))
  (if *mitsuko-settings*
      (progn
        t
        )
    (format t "Failed to load mitsuko-settings.~%"))
  )

(defun setup-display ()
  ""
  (setf (uiop:getenv "WAYLAND_DISPLAY")
        (x:bytes-to-string (qml:qml-get "compositor" "socketName")))
  )


(defun version ()
  (format t "Mitsuko Version is whatever~%"))
