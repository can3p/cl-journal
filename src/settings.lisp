(in-package :cl-user)
(defpackage cl-journal.settings
  (:use :cl)
  (:import-from :cl-journal.functions :exec :prompt-read :prompt-read-password :chdir)
  (:export :setup :get-login :get-password))

(in-package :cl-journal.settings)

(defvar *pre-commit-hook* (format nil "~&#!/usr/bin/env bash
cl-journal sync
git add posts.lisp
"))

(defun setup-env ()
  (chdir #P"/Users/dpetrov/test-drafts/")
  (setf cl-journal.lj-api::*livejournal-login* (get-login))
  (setf cl-journal.lj-api::*livejournal-password* (get-password cl-journal.lj-api::*livejournal-login*)))

(defun get-login () (exec "git config livejournal.login"))

(defun set-login ()
  (if (not (equal "" (get-login)))
      (progn
        (format t "Login is already set!~%")
        (get-login)
        )
      (progn
        (let ((login (prompt-read "Login name")))
          (exec (format nil "git config livejournal.login \"~a\"" login))
          login
          ))))

(defun get-password (login)
  (exec (format nil "security find-internet-password -a ~a -s www.livejournal.com -w" login)))

(defun set-password (login)
  (if (not (equal "" (get-password login)))
      (format t "Password is already set!~%")
      (progn
        (let ((password (prompt-read-password "Password")))
          (exec (format nil "security add-internet-password -a ~a -s www.livejournal.com -w '~A'" login password))
          login
          ))))

(defun setup-hook ()
  (if (y-or-n-p "Do you want to set up pre-commit hook to make posting easier?")
    (with-open-file (out ".git/hooks/pre-commit"
                         :direction :output
                         :if-exists :supersede)
      (with-standard-io-syntax
        (format out *pre-commit-hook*)
        (exec "chmod +x .git/hooks/pre-commit")))))

(defun setup ()
  (let ((login (set-login)))
    (set-password login)
    (setup-hook)
    ))
