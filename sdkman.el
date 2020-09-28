;;; sdkman.el --- Emacs Lisp Java Switcher with SDKMan. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Nicolas CHAPON
;;
;; Author: Nicolas CHAPON <https://github.com/nchapon>
;; Maintainer: Nicolas CHAPON <nchapon@gmail.com>
;; Created: September 27, 2020
;; Modified: September 28, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/nchapon/sdkman.el
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Emacs Java Switcher with SDKMan.
;;
;;; Code:

(defcustom sdkman-candidates-base-dir "~/.sdkman/candidates/java"
  "Java Candidates Directory"
  :group 'sdkman
  :type 'string
  :safe 'stringp)

;;
;; This function returns the list of installed
;;
(defun sdk--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..") (equal a "current") (equal a ".DS_Store")))
   (directory-files sdkman-candidates-base-dir)))


(defun sdk--save-env ()
Store original PATH and JAVA_HOME.
(when (not (boundp 'SW_JAVA_PATH))
  (setq SW_JAVA_PATH (getenv "PATH")))
(when (not (boundp 'SW_JAVA_HOME))
  (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))


(defun sdk-use-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (sdk--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (sdk--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat sdkman-candidates-base-dir "/" ver))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
                           ":" SW_JAVA_PATH)))
  ;; show version
  (sdk-current))


(defun sdk-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (sdk--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  ;; show version
  (sdk-current))


(defun sdk-current ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays current java version
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))


(provide 'sdkman)
