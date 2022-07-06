(in-package :cl-user)

(defpackage :mitsuko-core
  (:use :cl :eql)
  (:export
   #:*app-data-paths*
   #:*app-writable-path*
   #:*is-swank-available*
   #:*mitsuko-module*
   #:*qml-application-engine*
   #:*qml-search-path*
   #:*swank-path*
   #:*swank-port*
   #:*wayland-terminals*
   #:find-in-app-data
   #:find-in-path
   #:l
   #:run-command
   #:run-terminal
   #:shutdown
   #:start
   #:update-qml-import-path))

(in-package :mitsuko-core)
(require "asdf")
(qrequire :quick)

(defvar *app-data-paths* (|standardLocations.QStandardPaths| |QStandardPaths.DataLocation|))
(defvar *app-writable-path* (|writableLocation.QStandardPaths| |QStandardPaths.DataLocation|))
(defvar *is-swank-available* nil)
(defvar *mitsuko-module* nil)
(defvar *mitsuko-path* nil)
(defvar *qml-application-engine nil)
(defvar *qml-search-path* '("/usr/lib64/qml" "/usr/lib/qml"))
(defvar *swank-path* nil)
(defvar *swank-port* 4005)
(defvar *wayland-terminals* '("terminator" "qterminal" "kitty" "terminology" "alacritty"))

(defvar .mitsuko-initialized. nil)
(defvar .startup-timer. nil)
(defvar .shutdown-timer. nil)
(defvar .status-code. 0)

(defun find-in-path (program)
  "Try to locate an application in PATH"
  (if (uiop:getenvp"PATH")
      (let ((paths (uiop:split-string (uiop:getenv "PATH") :separator ":")))
        (loop for path in paths do
          (let ((file-path (concatenate 'string path "/" program)))
            (if (uiop:file-exists-p file-path)
                (return file-path)))))
      (format t "PATH variable not set, this might be a bit of an issue.")))

(defun find-in-app-data (file)
  "Try to locate a file in the applications default data path. Returns an
absolute path if found, nil if not."
  (let ((data-paths *app-data-paths*))
    (if (uiop:getenvp "MITSUKO_PATH")
        (let ((mitsuko_path_split (uiop:split-string (uiop:getenv "MITSUKO_PATH") :separator ":")))
          (setf data-paths (concatenate 'list mitsuko_path_split data-paths))))
    (if *mitsuko-path*
        (setf data-paths (concatenate 'list *mitsuko-path* data-paths)))
    (block nested
      (loop for path in data-paths
            do
               (loop for suffix in '("" "qml/" "lisp/")
                     do
                        (let ((file-path (concatenate 'string path "/" suffix file))
                              (directory-path (concatenate 'string path "/" suffix)))
                          (cond ((uiop:file-exists-p file-path)
                                 (format t "Found ~A in ~A.~%" file directory-path)
                                 (return-from nested file-path))
                                ((and *mitsuko-module*
                                      (uiop:file-exists-p (concatenate 'string directory-path "/" *mitsuko-module* "/" file)))
                                 (format t "Found ~A in ~A, module subdirectory ~A~%" file directory-path *mitsuko-module*)
                                 (return-from nested (concatenate 'string directory-path "/" *mitsuko-module* "/" file)))
                                (t
                                 (format t "No ~A in ~A, skipping.~%" file directory-path)))))))))

(defun shutdown (&optional exit-code)
  "Shut down the application - both the Qt and LISP parts. This uses a timer to
make sure Qt is running - otherwise the application would dead lock. Therefore
additional code execution before shutdown may still happen in rare instances."
  (format t "Shutting down mitsuko.~%")
  (if exit-code
      (setf .status-code. exit-code))
  (if (and .mitsuko-initialized. (fboundp 'mitsuko-pre-shutdown))
      (mitsuko-pre-shutdown))
  (setf .shutdown-timer. (qnew "QTimer" "singleShot" t "interval" 0))
  (qconnect .shutdown-timer. "timeout()" 'qapplication-exit)
  (|start| .shutdown-timer.))


(defun l(library &optional fatal)
  "Convenience function to (re)-load libraries from within mitsuko
data directories"
  (let ((app-data-library (find-in-app-data library)))
    (if app-data-library
        (qload app-data-library)
        (progn
          (format t "Library ~A not found. Consider setting MITSUKO_PATH.~%" library)
          (if fatal
              (shutdown -1))))))

(defun run-terminal()
  (let ((terminal-program
          (loop for terminal in *wayland-terminals* do
            (let ((terminal-path (find-in-path terminal)))
              (if terminal-path
                  (return terminal-path))))))
    (if terminal-program
        (uiop:launch-program terminal-program)
        (format t "No supported terminal found in PATH.~%"))))

;; TODO, this needs error handling, and should keep track of running processes
(defun run-command(command &optional path)
  "Run a command, either by trying to locate it in PATH, or from path"
  (if path
      (uiop:launch-program (concatenate 'string path "/" command))
      (let ((command-with-path (find-in-path command)))
        (if command-with-path
            (uiop:launch-program command-with-path)))))

(defun update-qml-import-path()
  "Add all directories which may contain QML imports to import path."
  (loop for path in *qml-search-path* do
    (if
     (and (probe-file path) (not (uiop:file-exists-p path)))
     (progn
       (format t "Adding ~A to QML search path.~%" path)
       (|addImportPath| *qml-application-engine* path))
     (format t "QML path ~A does not exist, skipping.~%" path))))

;;;; internal helpers
(defun qapplication-exit()
  "Internal helper to tear down everything."
  (|exit.QCoreApplication| .status-code.)
  ;; this should never be reached
  (cl-user::quit .status-code.))

(defun search-and-do-fun(package symbol)
  "Search and evaluate function symbol"
  (let ((fun (find-symbol symbol package)))
    (if fun
        (funcall fun))))

;;;; qml helpers
;; the qml generation is based on EQL5s palindrome example
(defmacro with-qml-file ((file) &body body)
  (let ((text (gensym)))
    `(let ((,text (with-output-to-string (s)
                    ,@(mapcar (lambda (x) (if (stringp x) `(write-line ,x s) x))
                              body))))
       ;; stream S is intentionally not a gensym
       (with-open-file (s ,file :direction :output :if-exists :supersede)
         (format s "// THIS FILE IS GENERATED~%~%")
         (write-string (%indent-qml ,text) s)
         (qlater (lambda () (format t "~%QML file generated, see ~S~%~%" ,file)))))))

(defmacro qml-entity (first &body body)
  (if (find #\~ first)
      `(progn
         (format s ,first ,@body)
         (terpri s))
      (let ((open-close (and (upper-case-p (char first 0))
                             (not (find #\{ first)))))
        (if body
            `(progn
               ,(if open-close
                    `(write-line ,(format nil "~%~A {" first) s)
                    (if (find #\{ first)
                        `(write-line ,(format nil "~%~A" first))
                        `(write-line ,first s)))
               ,@(mapcar (lambda (x)
                           (if (stringp x)
                               (if (x:starts-with "id:" x)
                                   `(progn
                                      (write-line ,x s)
                                      (write-line ,(format nil "objectName: ~S" (string-trim " " (subseq x 3))) s))
                                   `(write-line ,x s))
                               x))
                         body)
               ,(when open-close `(write-line "}" s)))
            (if (find #\{ first)
                `(write-line ,(format nil "~%~A" first) s)
                `(write-line ,first s))))))

(defun %indent-qml (text)
  (with-output-to-string (out)
    (let ((in (make-string-input-stream text))
          (depth 0))
      (x:while-it (read-line in nil nil)
        (let ((open (count #\{ x:it))
              (close (count #\} x:it)))
          (write-string (make-string (* 4 (- depth (if (= (- open close) -1) 1 0)))) out)
          (write-line (string-left-trim " " x:it) out)
          (setf depth (+ depth open (- close))))))))

(defun generate-vanilla-qml(qml-file-path)
  ;; this is pretty much just the minimal qml compositor example
  (with-qml-file (qml-file-path)
    "import QtQuick 2.12"
    "import QtQuick.Controls 2.12"
    "import QtQuick.Window 2.2"
    "import QtQuick.Layouts 1.3"
    "import QtWayland.Compositor 1.3"
    "import EQL5 1.0"
    (qml-entity
     "WaylandCompositor"
     "id: compositor"
     (qml-entity
      "WaylandOutput"
      "id: output"
      "sizeFollowsWindow: true"
      ;; window goes here
      "window:"
      (qml-entity
       "Window"
       "width: 500"
       "height: 500"
       "visible: true"
       "id: window"
       (qml-entity
        "Text"
        "color: \"red\""
        "font.pointSize: 24"
        "text: \"mitsuko in failsafe mode<br>left-click anywhere for menu\"")
       (qml-entity
        "MouseArea"
        "anchors.fill: parent"
        "onClicked: wmMenu.popup(mouseX, mouseY)")
       (qml-entity
        "Menu"
        "id: wmMenu"
        (qml-entity
         "MenuItem"
         "text: \"Terminal\""
         "onClicked: Lisp.call(\"mitsuko-core:run-terminal\");")
        (qml-entity
         "MenuItem"
         "text: \"Exit\""
         "onClicked: Lisp.call(\"mitsuko-core:shutdown\");"))
       (qml-entity
        "Repeater"
        "id: repeater"
        "model: shellSurfaces"
        (qml-entity
         "Column"
         "id: chrome"
         "width: shellSurfaceItem.implicitWidth"
         (qml-entity
          "Rectangle"
          "visible: modelData.toplevel.decorationMode === XdgToplevel.ServerSideDecoration"
          "width: parent.width"
          "height: 30"
          "gradient: \"HeavyRain\""
          (qml-entity
           "Text"
           "text: modelData.toplevel.title"
           "anchors.centerIn: parent")
          (qml-entity
           "Item"
           "anchors.right: parent.right"
           "width: 30"
           "height: 30"
           (qml-entity
            "Text"
            "text: \"X\""
            "anchors.centerIn: parent")
           (qml-entity
            "TapHandler"
            "onTapped: modelData.toplevel.sendClose()"))
          (qml-entity
           "DragHandler"
           "target: chrome"))
         (qml-entity
          "ShellSurfaceItem"
          "id: shellSurfaceItem"
          "autoCreatePopupItems: true"
          "shellSurface: modelData"
          "onSurfaceDestroyed: shellSurfaces.remove(index)")))))
     (qml-entity
      "XdgDecorationManagerV1"
      "preferredMode: XdgToplevel.ServerSideDecoration")
     (qml-entity
      "WlShell"
      "onWlShellSurfaceCreated: shellSurfaces.append({shellSurface: shellSurface});"
      "id: wlShell")
     (qml-entity
      "XdgShellV6"
      "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});"
      "id: xdgShellV6")
     (qml-entity
      "XdgShell"
      "onToplevelCreated: shellSurfaces.append({shellSurface: xdgSurface});"
      "id: xdgShell")
     (qml-entity
      "ListModel"
       "id: shellSurfaces"))))

;;;; init
(defun vanilla-load-module(module)
  "Load module specific setup."
  (if (fboundp 'mitsuko-pre-init-module)
      (mitsuko-pre-init-module)
      (progn
        ;; having display set confuses stuff when running on X
        (setf (uiop:getenv "DISPLAY") nil)

        (l "core/qml-lisp.lisp")
        (l "core/mitsuko.lisp")

        (when *is-swank-available*
          (l "core/swank.lisp")
          (create-swank-server))

        ;; to keep the compiler happy calling stuff in the mitsuko-compositor package
        ;; needs to happen through looking up the symbol + funcall
        (if (find-package :mitsuko-compositor)
            (progn
              (let ((module-init (find-symbol "INIT-MODULE" :mitsuko-compositor))
                    (module-flags (eval (find-symbol "*FLAGS*" :mitsuko-compositor))))
                (if (and module-flags (find-package :mitsuko))
                    (progn
                      (format t "Loading module flags.~%")
                      (cond ((member :settings-plugin module-flags)
                             (search-and-do-fun :mitsuko "LOAD-SETTINGS-PLUGIN")))))
                (if module-init
                    (progn
                      (format t "Found compositor init function.~%")
                      (funcall module-init))))))

        ;; TODO, split this out to something like set-compositor
        (|load| *qml-application-engine*
                (|fromLocalFile.QUrl| (find-in-app-data (concatenate 'string module ".qml"))))))
  (if (fboundp 'mitsuko-post-init-module)
      (mitsuko-post-init-module)
      (progn
        (if (find-package :mitsuko-compositor)
            (progn
              (let ((module-init (find-symbol "POST-INIT-MODULE" :mitsuko-compositor)))
                (if module-init
                    (progn
                      (format t "Found compositor post-init function.~%")
                      (funcall module-init)))))))))

(defun vanilla-pre-init()
  "The job of pre-init is to make sure there is a usable QQmlApplicationEngine
object available. Current assumption is that typically this vanilla pre-init
will be sufficient."
  (format t "Using vanilla pre-init.~%")
  ;; this triggers with a slow delay to increase chances of shutdown timers
  ;; running first
  (setf .startup-timer. (qnew "QTimer" "singleShot" t "interval" 10))
  (qconnect .startup-timer. "timeout()" 'vanilla-post-init)
  (|start| .startup-timer.)
  (unless (ensure-directories-exist (concatenate 'string *app-writable-path* "/"))
    (format t "Unable to create writable directory ~A - this might be problematic.~%"))
  (if (uiop:getenvp "MITSUKO_MODULE")
      (progn
        (setf *mitsuko-module* (uiop:getenv "MITSUKO_MODULE"))
        (format t "Setting ~A as mitsuko module.~%" *mitsuko-module*)))
  (setf *qml-application-engine* (qnew "QQmlApplicationEngine"))
  (update-qml-import-path)
  (format t "QML import path is now ~A.~%" (|importPathList| *qml-application-engine*)))

(defun vanilla-init()
  "The job of init is to load QML into the application engine, and do other
setup tasks. The vanilla init is just supposed to provide a minimal compositor
for rescue attempts if things went south."
  (if *mitsuko-module*
      (progn
        (l (concatenate 'string *mitsuko-module* ".lisp") t)
        (vanilla-load-module *mitsuko-module*))
      (progn
        (format t "Using vanilla init. This is probably not what you want.~%")
        (let ((qml-file (concatenate 'string *app-writable-path*
                                     "/mitsuko-vanilla-main.qml")))
          (format t "Generating ~A.~%" qml-file)
          (generate-vanilla-qml qml-file)
          (vanilla-load-module "mitsuko-vanilla-main")))))

(defun vanilla-post-init()
  "Post init function executed once the Qt part is up and running. This could be
used for starting initial applications in a session."
  (format t "mitsuko startup finished.~%")
  (if (fboundp 'mitsuko-post-init)
      (mitsuko-post-init)))

(defun start()
  (let ((init-file (find-in-app-data "mitsuko-init.lisp")))
    (if init-file
        (progn
          (qload init-file))
        (format t "No mitsuko init file found~%")))

  (unless *swank-path*
    (setf *swank-path* (find-in-app-data "slime/swank.asd")))

  ;; swank will get compiled - which will fail with the default options on
  ;; systems without cc installed. This switches to byte compilation, which
  ;; should always work. If needed it can be switched back later on using
  ;; (ext:install-c-compiler)
  (ext:install-bytecodes-compiler)

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
