(in-package :cl-user)
(defpackage cl-journal
  (:use :cl :cl-journal.lj-api :cl-journal.file-api)
  (:import-from :cl-journal.db <db> <post-file> <post> :create-db-from-list :filename :title :to-list :get-by-fname :read-from-file :draft :publish-post :update-post :get-modified :get-deleted :delete-post :url)
  (:export :*posts* :publish-new-files :publish-modified-files :unpublish-deleted-files :restore-posts :lookup-file-url))

(in-package :cl-journal)

(defvar *posts* nil)
(defvar *posts-file* "posts.lisp")

(defun save-posts ()
  (with-open-file (out *posts-file*
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (pprint (to-list *posts*) out))))

(defun restore-posts ()
  (if (probe-file *posts-file*)
      (with-open-file (in *posts-file*)
        (with-standard-io-syntax
          (setf *posts* (create-db-from-list (read in)))))))

(defun file-published-p (fname)
  (get-by-fname *posts* fname))

(defun get-new-files ()
  (remove-if #'draft
    (mapcar #'read-from-file
            (remove-if #'file-published-p (get-markdown-files)))))

(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))

(defmethod delete-post :after ((db <db>) (post <post>))
  (save-posts))

(defmethod update-post :after ((post <post>))
  (save-posts))

(defun lookup-file-url (fname)
  (let ((obj (get-by-fname *posts* fname)))
    (when obj (url obj))))

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
  (let ((deleted (get-deleted *posts*)))
    (if (> (length deleted) 0)
        (progn
          (if (> (length deleted) 1)
              (format t "There are ~a deleted files to unpublish~%" (length deleted))
              (format t "There a deleted file to unpublish~%"))
          (loop for post in deleted do
            (let ((prompt (format nil "Filename: ~a~%Url: ~a~%~%Unpublish this file?"
                                  (filename post)
                                  (cl-journal.db:url post))))
              (if (y-or-n-p prompt)
                  (delete-post *posts* post)))))
        (format t "No published files were deleted~%"))))
