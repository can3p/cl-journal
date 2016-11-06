(in-package :cl-user)
(defpackage cl-journal.main
  (:use :cl :cl-journal :cl-journal.settings :cl-journal.lj-api)
  (:import-from :cl-journal.functions :exec :cwd-is-git-dir-p)
  (:export :main))

(in-package :cl-journal.main)

(defun help ()
  (format t "~&Usage:
Livejournal client written in Common Lisp. Please run all commands in
a top level directory of your git repo. Right now client is supposed
to work on Mac OS X only.

Usage:
cl-journal.ros [command]

Commands:
    init
        Setup up a directory. Setup will ask for login name and password.
        Password will be stored in a default Mac OS X keychain.

    sync
        Find new posts and publish them. Drafts will be skipped client
        will prompt before publishing every file.

    url <file>
        Lookup url where file was published and print it
"))

(defun main (args)
  (apply #'main-entry (cdr args)))

(defun main-entry (&optional (command "help") (arg ()))
  (if (cwd-is-git-dir-p)
      (cond
        ((equal command "init") (setup))
        ((equal command "sync") (let* ((*livejournal-login* (get-login))
                                      (*livejournal-password* (get-password *livejournal-login*)))
                                 (if (or (equal "" *livejournal-login*)
                                         (equal "" *livejournal-password*)
                                         )
                                     (format t "Setup is not complete! Run cl-journal init from this folder")
                                     (progn
                                       (restore-posts)
                                       (publish-new-files)
                                       (publish-modified-files)
                                       (unpublish-deleted-files)
                                       )
                                     )))
        ((equal command "url") (if arg
                                   (progn
                                     (restore-posts)
                                     (let ((url (lookup-file-url arg)))
                                       (if url
                                           (format t "~a~%" url)
                                           (format t "File is not published~%"))))
                                   (format t "Please specify filename to lookup~%")))
        (t (help)))
      (progn
        (format t "Please run this command from top level directory in the git repo~%~%")
        (help)
        )))
