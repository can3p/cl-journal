;;;; cl-journal.lisp

(in-package #:cl-journal)

(defun create-post-from-file (filename)
  (create-post (parse-post-file filename)))
