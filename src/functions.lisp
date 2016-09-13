(in-package :cl-user)

(defpackage cl-journal.functions
  (:use :cl :uiop/run-program)
  (:export :get-date-struct))

(in-package :cl-journal.functions)

(defun get-date-struct (&optional ts)
  (multiple-value-bind
        (second minute hour date month year)
      (decode-universal-time (or ts (get-universal-time)))
    (declare (ignore second))

    (list :year year
          :mon month
          :day date
          :hour hour
          :min minute)))

(defun chdir (dir)
  (uiop/os::chdir dir)
  (setf *default-pathname-defaults* dir))

(defun cwd-is-git-dir-p ()
  (if (probe-file ".git") T NIL))

(defun prompt-read (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-password (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (let ((password (run-program "read -s val; echo $val" :output :string :input :interactive)))
    (string-trim '(#\Newline #\Space) password)
    ))

(defun exec (str)
  (let ((result (run-program str
                            :ignore-error-status t
                            :output :string)))
    (string-trim `(#\Newline) result)))

