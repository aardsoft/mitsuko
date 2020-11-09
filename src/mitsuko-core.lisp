(in-package :cl-user)

(defpackage :mitsuko
  (:use :cl :eql)
  (:export
   #:*app-data-paths*
   #:*app-writable-path*
   #:*is-swank-available*
   #:*swank-path*
   #:*swank-port*
   #:*qml-application-engine*
   #:find-in-app-data
   #:l
   #:start))

(in-package :mitsuko)
(require "asdf")
(require :qml-file "qml-file")
(qrequire :quick)

(defvar *qml-application-engine nil)
(defvar *app-data-paths* (|standardLocations.QStandardPaths| |QStandardPaths.DataLocation|))
(defvar *app-writable-path* (|writableLocation.QStandardPaths| |QStandardPaths.DataLocation|))
(defvar *is-swank-available* nil)
(defvar *swank-path* nil)
(defvar *swank-port* 4005)

(defun find-in-app-data (file)
  "Try to locate a file in the applications default data path. Returns an
absolute path if found, nil if not."
  (loop for path in *app-data-paths*
        do (let ((file-path (concatenate 'string path "/" file)))
             (if (uiop:file-exists-p file-path)
                 (progn
                   (format t "Found ~A in ~A.~%" file path)
                   (return file-path))
               (format t "No ~A in ~A, skipping.~%" file path)))))

(defun vanilla-pre-init()
  "The job of pre-init is to make sure there is a usable QQmlApplicationEngine
object available. Current assumption is that typically this vanilla pre-init
will be sufficient."
  (print "Using vanilla pre-init.")
  (setf *qml-application-engine* (qnew "QQmlApplicationEngine")))

(defun vanilla-init()
  "The job of init is to load QML into the application engine, and do other
setup tasks. The vanilla init is just supposed to provide a minimal compositor
for rescue attempts if things went south."
  (format t "Using vanilla init. This is probably not what you want.~%")

  (let ((qml-file (find-in-app-data "mitsuko-vanilla-main.qml")))
    (if qml-file
        (progn
          (format t "Using qml file ~A.~%" qml-file)
          (|load| *qml-application-engine* (|fromLocalFile.QUrl| qml-file)))
        (progn
          (let ((qml-file (concatenate 'string *app-writable-path*
                                       "/mitsuko-vanilla-main.qml")))
            (if (search "mitsuko" *app-writable-path*)
                (progn
                  (format t "No qml file found, trying to generate one in ~A.~%"
                          *app-writable-path*)
                  (generate-vanilla-qml qml-file)
                  (|load| *qml-application-engine* (|fromLocalFile.QUrl| qml-file)))
                (format t "No mitsuko in ~A, assuming compile time.~%"
                        *app-writable-path*)))))))

(defun generate-vanilla-qml(qml-file-path)
  ;; this is pretty much just the minimal qml compositor example
  (with-qml-file (qml-file-path)
    "import QtQuick 2.6"
    "import QtQuick.Window 2.2"
    "import QtWayland.Compositor 1.3"
    (qml "WaylandCompositor"
         "id: compositor"
         (qml "WaylandOutput"
              "id: output"
              "sizeFollowsWindow: true"
              ;; window goes here
              "window:"
              (qml "Window"
                   "width: 500"
                   "height: 500"
                   "visible: true"
                   "id: window"
                   (qml "Repeater"
                        "id: repeater"
                        "model: shellSurfaces"
                        (qml "ShellSurfaceItem"
                             "id: shellSurfaceItem"
                             "autoCreatePopupItems: true"
                             "shellSurface: modelData"
                             "onSurfaceDestroyed: shellSurfaces.remove(index)"))))
         (qml "WlShell"
              "onWlShellSurfaceCreated: shellSurfaces.append({shellSurface: shellSurface});"
              "id: wlShell")
         (qml "XdgShellV6"
              "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});"
              "id: xdgShellV6")
         (qml "XdgShell"
              "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});"
              "id: xdgShell")
         (qml "ListModel"
              "id: shellSurfaces"))))

(defun l(library)
  "Convenience function to load libraries from within mitsuko
data directories"
  (qload (find-in-app-data library)))

(defun start()
  (let ((init-file (find-in-app-data "mitsuko-init.lisp")))
    (if init-file
        (progn
          (qload init-file))
        (print "No mitsuko init file found")))

  (unless *swank-path*
    (setf *swank-path* (find-in-app-data "slime/swank.asd")))

  (when *swank-path*
      (push (uiop:pathname-directory-pathname *swank-path*)
            asdf:*central-registry*)
      (asdf:load-system :swank)
      (setf *is-swank-available* t))

  (if (fboundp 'mitsuko-pre-init)
      (mitsuko-pre-init)
      (vanilla-pre-init))

  (if (fboundp 'mitsuko-init)
      (mitsuko-init)
      (vanilla-init)))

(start)
