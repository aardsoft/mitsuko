(in-package :mitsuko-core)

(defun create-swank-server()
  (setf swank:*globally-redirect-io* t)
  (swank:create-server :port *swank-port*
                       :style :spawn
                       :dont-close t))
