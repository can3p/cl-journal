(in-package :cl-user)
(defpackage cl-journal.main
  (:use :cl :cl-journal)
  (:import-from :cl-journal.functions
                :cwd-has-posts-file-p
                )
  (:export :main))

(in-package :cl-journal.main)

(defun help ()
  (format t "~&
Livejournal/Dreamwidth client written in Common Lisp. Version: ~a

Please run all commands in a top level directory of your git repo.

Usage:
cl-journal [command]

Commands:
    init [service]
        Setup up a directory. Setup will ask for login name and password.
        Password will be stored in a default Mac OS X keychain.

        Optionally a service name can be passed. Currently only
        livejournal and dreamwidth are supported, but any other
        service name will be treated as an xml-rpc endpoint
        to be used. Default is livejournal

    push
        Find new posts and publish them. Drafts will be skipped client
        will prompt before publishing every file.

    status
        Display the list with filenames of all drafts or files that
        are out of sync with blog service.

    ignore-all
        Sometimes timestamps on files go out of sync with database and
        no real update is necessary. This command will update database
        to treat all posts as up to date

    new <name>
        Open editor to edit a file like yyyy-mm-dd-<name>.md, where the date
        is a current date. if name has slashes like a/name file a/yyyy-mm-dd-name.md
        will be opened
    last
        Open editor to edit last published post

    url <file>
        Lookup url where file was published and print it
" (asdf::component-version (asdf::find-system :cl-journal))))

(defun main (args)
  (apply #'main-entry (cdr args)))

(defun main-entry (&optional (command "help") (arg ()))
  (if (or (cwd-has-posts-file-p) (equal command "init"))
      (cond
        ((equal command "init") (setup (string-downcase
                                        (or arg "livejournal"))))
        ((equal command "push") (progn
                                  (restore-posts)
                                  (publish-new-files)
                                  (publish-modified-files)
                                  (unpublish-deleted-files)
                                  ))
        ((equal command "url") (if arg
                                   (progn
                                     (restore-posts)
                                     (let ((url (lookup-file-url arg)))
                                       (if url
                                           (format t "~a~%" url)
                                           (format t "File is not published~%"))))
                                   (format t "Please specify filename to lookup~%")))
        ((equal command "status") (progn
                                  (restore-posts)
                                  (print-status)))
        ((equal command "ignore-all") (progn
                                  (restore-posts)
                                  (ignore-all)))
        ((equal command "last") (progn
                                  (restore-posts)
                                  (edit-last-published-post)))
        ((equal command "new") (progn
                                    (edit-new-post arg)))
        (t (help)))
      (progn
        (format t "Please run this command from top level directory of your journal (it's where db file is located)~%~%")
        (help)
        )))
