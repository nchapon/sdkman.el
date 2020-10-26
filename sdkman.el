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


(setq sdkman-init-shell "~/.sdkman/bin/sdkman-init.sh")


;;
;; This function returns the list of installed
;;
(defun sdk--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..") (equal a "current") (equal a ".DS_Store")))
   (directory-files sdkman-candidates-base-dir)))


(defun sdk-use-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (sdk--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (expand-file-name (concat sdkman-candidates-base-dir "/" ver)))
    (eshell/addpath (concat (getenv "JAVA_HOME") "/bin/java")))
  ;; show version
  (sdk-current))


(defun sdk-current ()
  "Display the current selected Java version."
  (interactive)
  ;; displays current java version
  (message (sdk-command "sdk current java")))


(defun sdk-command (cmd)
    "DOCSTRING"
  (interactive)
  (shell-command-to-string (concat "source " (expand-file-name sdkman-init-shell) " && " cmd)))



(provide 'sdkman)
