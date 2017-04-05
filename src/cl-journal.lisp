(in-package :cl-user)
(defpackage cl-journal
  (:use :cl :cl-arrows
        :split-sequence
        :cl-journal.lj-api
        :cl-journal.settings
        :cl-journal.db
        :cl-journal.functions
        :cl-journal.file-api)
  (:export :*posts*
           :setup
           :publish-new-files
           :publish-modified-files
           :unpublish-deleted-files
           :restore-posts
           :lookup-file-url
           :edit-new-post
           :print-status
           :edit-last-published-post))

(in-package :cl-journal)

(defvar *posts* nil)
(defvar *posts-file* "posts.lisp")

;; update db after any actions on posts
(defmethod publish-post :after ((db <db>) (post-file <post-file>))
  (save-posts))

(defmethod delete-post :after ((db <db>) (post <post>))
  (save-posts))

(defmethod update-post :after ((db <db>) (post <post>))
  (save-posts))

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

(defun setup ()
  (if (restore-posts) (format t "db file exists, remove it to recreate~%")
      (progn
        (setf *posts* (create-empty-db))
        (save-posts)))
  (setup-settings *posts*))

(defun edit-new-post (name)
  (let* ((parts (reverse (split-sequence #\/ name)))
         (name (car parts))
         (rest (reverse (cdr parts)))
         (date (get-date-struct))
         (filename (format nil "~{~a/~}~a-~2,'0d-~2,'0d-~a.md"
                           rest
                           (getf date :year)
                           (getf date :mon)
                           (getf date :day)
                           name
                           )))
    (magic-ed:magic-ed filename :eval nil)))

(defun edit-last-published-post ()
  (let ((post (get-last-published-post *posts*)))
    (if post
        (magic-ed:magic-ed (filename post) :eval nil)
        (format t "You don't have any published posts yet~%"))))

(defun get-new-files ()
  (->> (get-markdown-files)
       (remove-if #'(lambda (fname) (get-by-fname *posts* fname)))
       (mapcar #'read-from-file)
       (remove-if #'draft)))

(defun get-draft-files ()
  (->> (get-markdown-files)
       (remove-if #'(lambda (fname) (get-by-fname *posts* fname)))
       (mapcar #'read-from-file)
       (remove-if-not #'draft)
       (mapcar #'filename)
       ))

(defun lookup-file-url (fname)
  (let ((obj (get-by-fname *posts* fname)))
    (when obj (url obj))))

(defmacro with-files (name accessor multiplemsg singlemsg nomsg)
  (let ((fn-name (intern
                  (concatenate 'string
                               "WITH-"
                               (symbol-name name)
                               "-FILES"))))
    `(defun ,fn-name (cb)
       (let ((items ,accessor))
         (if (> (length items) 0)
             (progn
               (if (> (length items) 1)
                   (format t ,multiplemsg (length items))
                   (format t ,singlemsg))
               (funcall cb items))
             (format t ,nomsg))))))

(with-files new (get-new-files)
  "There are ~a new files to publish~%"
  "There is a new file to publish~%"
  "No new files to publish~%")

(with-files modified (get-modified *posts*)
  "There are ~a modified files to update~%"
  "There a modified file to update~%"
  "No published files were modified~%")

(with-files deleted (get-deleted *posts*)
  "There are ~a deleted files to unpublish~%"
  "There is a deleted file to unpublish~%"
  "No published files were deleted~%")

(with-files draft (get-draft-files)
  "There are ~a drafts files~%"
  "There a drafts file~%"
  "No drafts found~%")

;; some duplication is still there, right?
(defun print-status ()
  (flet ((print-names (items)
           (format t "~%~{    ~a~^~%~}~%~%" (mapcar #'filename items)))
         (print-string-names (items)
           (format t "~%~{    ~a~^~%~}~%~%" items))
         )
    (with-draft-files #'print-string-names)
    (with-new-files #'print-names)
    (with-modified-files #'print-names)
    (with-deleted-files #'print-names)))

;; and there, yeah
(defun publish-new-files ()
  (with-new-files #'(lambda (items)
                      (loop for post in items do
                        (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Publish this file?"
                                              (filename post)
                                              (title post))))
                          (if (y-or-n-p prompt)
                              (publish-post *posts* post)))))))

(defun publish-modified-files ()
  (with-modified-files #'(lambda (items)
                           (loop for post in items do
                             (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Upload updated version of this file?"
                                                   (filename post)
                                                   (title post))))
                               (if (y-or-n-p prompt)
                                   (update-post *posts* post)))))))

(defun unpublish-deleted-files ()
  (with-deleted-files #'(lambda (items)
                          (loop for post in items do
                            (let ((prompt (format nil "Filename: ~a~%Url: ~a~%~%Unpublish this file?"
                                                  (filename post)
                                                  (cl-journal.db:url post))))
                              (if (y-or-n-p prompt)
                                  (delete-post *posts* post)))))))
