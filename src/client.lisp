(in-package #:client)

(defvar *pre-commit-hook* (format nil "~&#!/usr/bin/env bash
cl-journal sync
git add posts.lisp
"))

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

(defun setup-env ()
  (uiop/os::chdir #P"/Users/dpetrov/test-drafts/")
  (setf *default-pathname-defaults* #P"/Users/dpetrov/test-drafts/")
  (setf lj-api::*livejournal-login* (get-login))
  (setf lj-api::*livejournal-password* (get-password lj-api::*livejournal-login*))
  )

(defun get-login ()
  (let ((login (run-program "git config livejournal.login"
               :ignore-error-status t
               :output :string)))
    (string-trim `(#\Newline) login)))

(defun set-login ()
  (if (not (equal "" (get-login)))
      (progn
        (format t "Login is already set!~%")
        (get-login)
        )
      (progn
        (let ((login (prompt-read "Login name")))
          (run-program (format nil "git config livejournal.login \"~a\"" login)
                       :output :string)
          login
          ))))

(defun get-password (login)
  (let ((password (run-program (format nil "security find-internet-password -a ~a -s www.livejournal.com -w" login)
               :ignore-error-status t
               :output :string)))
    (string-trim `(#\Newline) password)))

(defun set-password (login)
  (if (not (equal "" (get-password login)))
      (format t "Password is already set!~%")
      (progn
        (let ((password (prompt-read-password "Password")))
          (run-program (format nil "security add-internet-password -a ~a -s www.livejournal.com -w '~A'" login password)
                       :ignore-error-status t
                       :output :string)
          login
          ))))

(defun setup-hook ()
  (if (y-or-n-p "Do you want to set up pre-commit hook to make posting easier?")
    (with-open-file (out ".git/hooks/pre-commit"
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (format out *pre-commit-hook*)
        (run-program "chmod +x .git/hooks/pre-commit")))))

(defun setup ()
  (let ((login (set-login)))
    (set-password login)
    (setup-hook)
    ))
