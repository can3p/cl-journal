(in-package :cl-user)
(defpackage cl-journal.settings
  (:use :cl :cl-journal.functions)
  (:import-from :cl-journal.db
                :url
                :service-url
                :login)
  (:export :setup-settings
           :get-password))

(in-package :cl-journal.settings)

(defvar *pre-commit-hook* (format nil "~&#!/usr/bin/env bash
cl-journal push
git add posts.lisp
"))

(defun get-password-cmd (login url)
  #-LINUX
  (format nil "security find-internet-password -a ~a -s ~a -w" login url)
  #+LINUX
  (format nil "secret-tool lookup '~a:login' ~a" url login))

#-LINUX
(defun set-password-cmd (login url password)
  (format nil "security add-internet-password -a ~a -s ~a -w '~A'" login url password))

#+LINUX
(defun set-password-cmd (login url)
  (format nil "secret-tool store --label='~a' '~a:login' '~a'" url url login))

(defun setup-dev-env ()
  (chdir #P"/Users/dpetrov/test-drafts/"))

(defun get-password-no-set (login url)
  (exec (get-password-cmd login url)))

(defun get-password (login url)
  (let ((password (get-password-no-set login url)))
    (if (not (equal "" password)) password
        (set-password login url))))

(defun set-password (login url)
  (if (not (equal "" (get-password-no-set login url)))
      (format t "Password is already set!~%")
      #-LINUX
      (progn
        (let ((password (prompt-read-password "Password")))
          (exec (set-password-cmd login url password))
          login
          ))
      #+LINUX
      (progn
        (exec-interactive (set-password-cmd login url))
        (get-password-no-set login url))))

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
  (set-password (login db) (service-url db))
  (setup-hook))
