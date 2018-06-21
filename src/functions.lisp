(in-package :cl-user)

(defpackage cl-journal.functions
  (:use :cl :uiop/run-program)
  (:import-from :local-time
   :format-timestring
   :unix-to-timestamp
   :+utc-zone+
   )
  (:export :get-date-struct
           :cwd-is-git-dir-p
           :cwd-has-posts-file-p
           :exec
           :exec-interactive
           :prompt-read
           :prompt-read-password
           :format-unix-timestamp
           :chdir))

(in-package :cl-journal.functions)

;; defconstant generated restarts
;; and I ended up with special variable instead
(defparameter *livejournal-timestamp-format* (list
                                 :year
                                 "-"
                                 (list :month 2)
                                 "-"
                                 (list :day 2)
                                 " "
                                 (list :hour 2)
                                 ":"
                                 (list :min 2)
                                 ":"
                                 (list :sec 2)
                                 ))

(defun format-unix-timestamp (ts)
  (format-timestring nil (unix-to-timestamp ts)
                     :format *livejournal-timestamp-format*
                      ;; looks like utc zone is what syncitems use
                     :timezone +utc-zone+))

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

(defun cwd-has-posts-file-p ()
  (if (probe-file "posts.lisp") T NIL))

(defun prompt-read (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-read-password (x)
  (format *query-io* "~a: " x)
  (force-output *query-io*)
  (let ((password (run-program "stty -echo; read val; stty echo; echo $val" :output :string :input :interactive)))
    (string-trim '(#\Newline #\Space) password)
    ))

(defun exec-interactive (str)
  (let ((result (run-program str
                            :input :interactive
                            :output :string)))
    (string-trim `(#\Newline #\Space) result)))

(defun exec (str)
  (let ((result (run-program str
                            :ignore-error-status t
                            :output :string)))
    (string-trim `(#\Newline) result)))



