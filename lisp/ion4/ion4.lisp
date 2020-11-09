(in-package :mitsuko)

(l "core/qml-lisp.lisp")

(when *is-swank-available*
  (l "core/swank.lisp")
  (create-swank-server))

(|load| *qml-application-engine*
        (|fromLocalFile.QUrl| (find-in-app-data "qml/ion4.qml")))
