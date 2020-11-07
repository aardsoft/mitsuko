(in-package :cl-user)

(defpackage :mitsuko
  (:use :cl :eql)
  (:export
   #:*qml-application-engine*
   #:start))

(in-package :mitsuko)
(require "asdf")
(require :qml-file "qml-file")
(qrequire :quick)

(defvar *qml-application-engine nil)
(defvar app-data-paths (|standardLocations.QStandardPaths| |QStandardPaths.DataLocation|))
(defvar app-writable-path (|writableLocation.QStandardPaths| |QStandardPaths.DataLocation|))

(defun find-in-app-data (file)
  "Try to locate a file in the applications default data path. Returns an
absolute path if found, nil if not."
  (loop for path in app-data-paths
        do (let ((file-path (concatenate 'string path "/" file)))
             (if (uiop:file-exists-p file-path)
                 (progn
                   (format t "Found ~A in ~A.~%" file path)
                   (return file-path))
               (format t "No ~A in ~A, skipping.~%" file path)))))

(defun mitsuko-vanilla-pre-init()
  "The job of pre-init is to make sure there is a usable QQmlApplicationEngine
object available. Current assumption is that typically this vanilla pre-init
will be sufficient."
  (print "Using vanilla pre-init.")
  (setf *qml-application-engine* (qnew "QQmlApplicationEngine")))

(defun mitsuko-vanilla-init()
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
          (let ((qml-file (concatenate 'string app-writable-path
                                       "/mitsuko-vanilla-main.qml")))
            (if (search "mitsuko" app-writable-path)
                (progn
                  (format t "No qml file found, trying to generate one in ~A.~%"
                          app-writable-path)
                  (generate-vanilla-qml qml-file)
                  (|load| *qml-application-engine* (|fromLocalFile.QUrl| qml-file)))
                (format t "No mitsuko in ~A, assuming compile time.~%"
                        app-writable-path)))))))

(defun generate-vanilla-qml(qml-file-path)
  ;; this is pretty much just the minimal qml compositor example
  (with-qml-file (qml-file-path)
    "import QtQuick 2.6"
    "import QtQuick.Window 2.2"
    "import QtWayland.Compositor 1.3"
    (qml "WaylandCompositor"
         (qml "WaylandOutput"
              "sizeFollowsWindow: true"
              ;; window goes here
              "window:"
              (qml "Window"
                   "width: 500"
                   "height: 500"
                   "visible: true"
                   (qml "Repeater"
                        "model: shellSurfaces"
                        (qml "ShellSurfaceItem"
                             "autoCreatePopupItems: true"
                             "shellSurface: modelData"
                             "onSurfaceDestroyed: shellSurfaces.remove(index)"))))
         (qml "WlShell"
              "onWlShellSurfaceCreated: shellSurfaces.append({shellSurface: shellSurface});")
         (qml "XdgShellV6"
              "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});")
         (qml "XdgShell"
              "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});")
         (qml "ListModel"
              "id: shellSurfaces"))))

(defun start()
  (let ((init-file (find-in-app-data "mitsuko-init.lisp")))
    (if init-file
        (progn
          (qload init-file))
        (print "No mitsuko init file found")))

  (if (fboundp 'mitsuko-pre-init)
      (mitsuko-pre-init)
      (mitsuko-vanilla-pre-init))

  (if (fboundp 'mitsuko-init)
      (mitsuko-init)
      (mitsuko-vanilla-init)))

(start)
