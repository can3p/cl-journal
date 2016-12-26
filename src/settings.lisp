(in-package :cl-user)
(defpackage cl-journal.settings
  (:use :cl :cl-journal.functions)
  (:import-from :cl-journal.db
                :login)
  (:export :setup-settings
           :get-password))

(in-package :cl-journal.settings)

(defvar *pre-commit-hook* (format nil "~&#!/usr/bin/env bash
cl-journal push
git add posts.lisp
"))

(defparameter +get-password-cmd+ "security find-internet-password -a ~a -s www.livejournal.com -w")

(defun setup-dev-env ()
  (chdir #P"/Users/dpetrov/test-drafts/"))

(defun get-password-no-set (login)
  (exec (format nil +get-password-cmd+ login)))

(defun get-password (login)
  (let ((password (get-password-no-set login)))
    (if (not (equal "" password)) password
        (set-password login))))

(defun set-password (login)
  (if (not (equal "" (get-password-no-set login)))
      (format t "Password is already set!~%")
      (progn
        (let ((password (prompt-read-password "Password")))
          (exec (format nil "security add-internet-password -a ~a -s www.livejournal.com -w '~A'" login password))
          login
          ))))

(defun setup-hook ()
  (if (y-or-n-p "Do you want to set up pre-commit hook to make posting easier?")
      (if (cwd-is-git-dir-p)
          (with-open-file (out ".git/hooks/pre-commit"
                               :direction :output
                               :if-exists :supersede)
            (with-standard-io-syntax
              (format out *pre-commit-hook*)
              (exec "chmod +x .git/hooks/pre-commit")))
          (format t "Cannot do! You don't have git repo in current working dir~%"))))

(defun setup-settings (db)
  (set-password (login db))
  (setup-hook))
