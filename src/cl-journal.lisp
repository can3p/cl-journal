(in-package :cl-user)
(defpackage cl-journal
  (:use :cl :cl-journal.lj-api :cl-journal.file-api :s-xml-rpc :cl-markdown)
  (:import-from :uiop/os :getcwd)
  (:import-from :cl-journal.functions :get-date-struct)
  (:import-from :cl-journal.db <db> <post-file> <post> :create-db-from-list :filename :title :to-list :get-by-fname :read-from-file :draft :publish-post :update-post :get-modified :get-deleted)
  (:export :*posts* :publish-new-files :publish-modified-files :unpublish-deleted-files :restore-posts :lookup-file-url))

(in-package :cl-journal)

(defvar *posts* nil)
(defvar *posts-file* "posts.lisp")

(defun save-posts ()
  (with-open-file (out *posts-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint *posts* out))))

(defun restore-posts ()
  (if (probe-file *posts-file*)
      (with-open-file (in *posts-file*)
        (with-standard-io-syntax
          (setf *posts* (create-db-from-list (read in)))))))

(defun get-markdown-files ()
  (mapcar #'(lambda (x) (enough-namestring x (getcwd)))
          (directory "./**/*.md")))

(defun file-published-p (fname)
  (get-by-fname *posts* fname))

(defun get-new-files ()
  (remove-if #'draft
    (mapcar #'read-from-file
            (remove-if #'file-published-p (get-markdown-files)))))

(defun get-deleted-files ()
  (remove-if #'(lambda (post) (probe-file (getf post :filename)))
             *posts*))

(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))

(defmethod update-post :after ((post <post>))
  (save-posts))

(defun lookup-file-url (fname)
  (let ((obj (file-published-p fname)))
    (if obj
    (getf (car obj) :url))))

(defun publish-new-files ()
  (let ((new (get-new-files)))
    (if (> (length new) 0)
        (progn
          (if (> (length new) 1)
              (format t "There are ~a new files to publish~%" (length new))
              (format t "There a new file to publish~%"))
          (loop for post in new do
            (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Publish this file?"
                                  (filename post)
                                  (title post))))
              (if (y-or-n-p prompt)
                  (publish-post *posts* post)))))
        (format t "No new files to publish~%"))))

(defun publish-modified-files ()
  (let ((modified (get-modified *posts*)))
    (if (> (length modified) 0)
        (progn
          (if (> (length modified) 1)
              (format t "There are ~a modified files to update~%" (length modified))
              (format t "There a modified file to update~%"))
          (loop for post in modified do
            (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Upload updated version of this file?"
                                  (filename post)
                                  (title post))))
              (if (y-or-n-p prompt)
                  (update-post post)))))
        (format t "No published files were modified~%"))))

(defun unpublish-deleted-files ()
  (let ((modified (get-deleted-files)))
    (if (> (length modified) 0)
        (progn
          (if (> (length modified) 1)
              (format t "There are ~a deleted files to unpublish~%" (length modified))
              (format t "There a deleted file to unpublish~%"))
          (loop for post in modified do
            (let ((prompt (format nil "Filename: ~a~%Url: ~a~%~%Unpublish this file?"
                                  (getf post :filename)
                                  (getf post :url))))
              (if (y-or-n-p prompt)
                  (unpublish-post-from-object-and-update-db post)))))
        (format t "No published files were deleted~%"))))

(defun unpublish-post-from-object-and-update-db (post)
  (delete-post post)
  (setf *posts* (remove-if #'(lambda (old-post)
                               (equal (getf old-post :itemid)
                                      (getf post :itemid))) *posts*))
  (save-posts))
