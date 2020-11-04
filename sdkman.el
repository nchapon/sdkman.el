(defcustom JAVA_BASE "~/.sdkman/candidates/java"

  "Java Candidates Directory"
  :group 'sdkman
  :type 'string
  :safe 'stringp)


(defun sdk--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..") (equal a "current") (equal a ".DS_Store")))
   (directory-files JAVA_BASE)))


(defun sdk-use-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (sdk--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (expand-file-name (concat JAVA_BASE "/" ver)))
    (setenv "PATH" (concat (getenv "JAVA_HOME") "/bin/java")))

  (sdk-current-java))

(defun sdk-current-java ()
  (interactive)
  (or
   (when (getenv "JAVA_HOME")
     (message (shell-command-to-string (concat "realpath " (getenv "JAVA_HOME")))))
   (message "JAVA_HOME not set")))


(provide 'sdkman)
