(defpackage :mitsuko-settings
  (:use :cl :eql :mitsuko-core)
  (:export
   #:*mitsuko-settings*
   #:get-bool
   #:get-int
   #:get-string
   #:get-string-list
   #:set-bool
   #:set-int
   #:set-string
   #:set-string-list
   #:set-notify
   ))

(in-package :mitsuko-settings)
;; this should be increased to the latest configuration version on changes
(defvar .configuration-version. 2)
(defvar .default-settings.
      '(1
        (("core" .
          (("version" int 2)
           ("statusBarZ" int 9000)))
         ("leftStatusBar" .
          (("visible" bool nil)
           ("color" string "black")
           ("width" int 20)))
         ("rightStatusBar" .
          (("visible" bool nil)
           ("color" string "black")
           ("width" int 20)))
         ("topStatusBar" .
          (("visible" bool nil)
           ("color" string "black")
           ("width" int 20)))
         ("bottomStatusBar" .
          (("visible" bool t)
           ("color" string "black")
           ("width" int 20))))
        2
        (("core" .
          (("version" int 2))))))

(defun get-bool(key &optional default-value)
  "Read a setting from key and convert it to a bool"
  (! "settingBool" (:qt mitsuko:*mitsuko-settings*) key default-value))

(defun get-int(key &optional default-value)
  "Read a setting from key and convert it to an integer"
  (! "settingInt" (:qt mitsuko:*mitsuko-settings*) key default-value))

(defun get-string(key &optional default-value)
  "Read a setting from key and convert it to string"
  (! "settingString" (:qt mitsuko:*mitsuko-settings*) key default-value))

(defun get-string-list(key &optional default-value)
  "Read a setting from key and convert the value to a string list"
  (! "settingStringList" (:qt mitsuko:*mitsuko-settings*) key default-value))


(defun set-bool(key value)
  "Set a settings key with a bool as value"
  (! "setSettingBool" (:qt mitsuko:*mitsuko-settings*) key value))

(defun set-int(key value)
  "Set a settings key with an integer as value"
  (! "setSettingInt" (:qt mitsuko:*mitsuko-settings*) key value))

(defun set-string(key value)
  "Set a settings key with a string as value. Note that converting from a symbol
somewhat works - but will end in an uppercase string"
  (! "setSettingString" (:qt mitsuko:*mitsuko-settings*) key value))

(defun set-string-list(key value)
  "Set a settings key with a string list as value"
  (! "setSettingStringList" (:qt mitsuko:*mitsuko-settings*) key value))

;;;
(defun set-notify(value)
  "Set or disable the signal changed notification"
  (! "setNotify" (:qt mitsuko:*mitsuko-settings*) value))

(defun set-default-settings()
  "Make sure default configuration exists. For newer configuration versions
just apply changes added since the running configuration version"
  (set-notify nil)
  (let ((config-version (get-int "core/version" 1)))
    (unless (= config-version .configuration-version.)
      (loop for i from config-version to .configuration-version. do
        (let ((config (getf .default-settings. i)))
          (if config
              (progn
                (format t "Processing version ~A~%" i)
                (loop for section in config do
                  (let ((section-name (car section))
                        (section-values (cdr section)))
                    (format t "Processing section ~A with ~A~%" section-name section-values)
                    (loop for key-value in section-values do
                      (let ((key (format nil "~A/~A" section-name (car key-value)))
                            (value (caddr key-value))
                            (type (cadr key-value)))
                        (format t "Processing ~A of type ~A with value ~A~%" key type value)
                        (cond ((eq type 'int)
                               (set-int key value))
                              ((eq type 'bool)
                               (set-bool key value))
                              ((eq type 'string)
                               (set-string key value))
                              (t (format t "Unhandled type ~A for key ~A~%" type key)))))))))))))
  (set-notify t))

(set-default-settings)
