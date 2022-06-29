(in-package :mitsuko-core)

(defun setup-display ()
  ""
  (setf (uiop:getenv "WAYLAND_DISPLAY")
        (x:bytes-to-string (qml:qml-get "compositor" "socketName")))
  )


(defun version ()
  (format t "Mitsuko Version is whatever~%"))
