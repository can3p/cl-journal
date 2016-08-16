;;;; cl-journal.lisp

(in-package #:cl-markdown)

(defmethod render-span-to-html :before
    ((code (eql 'inline-link)) body encoding-method)
  (let ((record (find-if #'(lambda (x) (equal (getf x :filename)
                                              (cadr body)))
                         cl-journal::*posts*
                         )))
    (if record
        (setf (cadr body) (getf record :url)))))

(defextension (lj-user :arguments ((name :required)) :insertp t)
  (setf name (ensure-string name))
  (let ((safe-name (html-safe-name name)))
    (ecase phase
      (:parse)
      (:render
       (format nil "<lj user='~a'>" safe-name)))))

(push 'lj-user *render-active-functions*)

(in-package #:cl-journal)

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
          (setf *posts* (read in))))))

(defun file-published-p (fname)
  (remove-if-not
   #'(lambda (post) (equal (getf post :filename) fname)) *posts*))

(defun file-draft-p (parsed-file)
  (getf parsed-file :draft))

(defun file-modified-p (file)
  (if file
      (and
       (probe-file (getf file :filename))
       (< (or (getf file :updated-at) (getf file :created-at))
          (file-write-date (getf file :filename))))))

(defun top-git-dir-p ()
  (probe-file ".git"))

(defun get-markdown-files ()
  (mapcar #'(lambda (x) (enough-namestring x (getcwd)))
          (directory "./**/*.md")))

(defun get-new-files ()
  (remove-if #'file-draft-p
             (mapcar #'(lambda (x)
                         (decorate-with-context
                          (parse-post-file x) x (get-universal-time)))
                     (remove-if #'file-published-p (get-markdown-files)))))

(defun get-modified-files ()
  (remove-if-not #'file-modified-p *posts*))

(defun get-deleted-files ()
  (remove-if #'(lambda (post) (probe-file (getf post :filename)))
             *posts*))

(defun publish-post-from-object (post-object)
  (let ((post (create-post-from-object post-object)))
    (push post *posts*)
    (save-posts)
    post))

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
          (loop for post in (get-new-files) do
            (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Publish this file?"
                                  (getf post :filename)
                                  (getf post :title))))
              (if (y-or-n-p prompt)
                  (publish-post-from-object post)))))
        (format t "No new files to publish~%"))))

(defun publish-modified-files ()
  (let ((modified (get-modified-files)))
    (if (> (length modified) 0)
        (progn
          (if (> (length modified) 1)
              (format t "There are ~a modified files to update~%" (length modified))
              (format t "There a modified file to update~%"))
          (loop for post in modified do
            (let ((prompt (format nil "Filename: ~a~%Title: ~a~%~%Upload updated version of this file?"
                                  (getf post :filename)
                                  (getf (parse-post-file (getf post :filename)) :title))))
              (if (y-or-n-p prompt)
                  (update-post-from-object-and-update-db post)))))
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

(defun create-post-from-object (post-object)
  (let ((ts (get-universal-time))
        (filename (getf post-object :filename))
        (post (create-post post-object)))
    (decorate-with-context
     (process-post-struct post)
     filename
     ts)))

(defun create-post-from-file (filename)
  (let ((ts (get-universal-time))
        (post (create-post (parse-post-file filename))))
    (decorate-with-context
     (process-post-struct post)
     filename
     ts)))

(defun update-post-from-object-and-update-db (post)
  (let ((new-post (update-post-from-object post)))
    (setf *posts* (remove-if #'(lambda (old-post) (equal (getf old-post :itemid)
                                                         (getf new-post :itemid))) *posts*))
    (push new-post *posts*)
    (save-posts)))

(defun unpublish-post-from-object-and-update-db (post)
  (delete-post post)
  (setf *posts* (remove-if #'(lambda (old-post)
                               (equal (getf old-post :itemid)
                                      (getf post :itemid))) *posts*))
  (save-posts))

(defun update-post-from-object (post)
  (let* ((ts (get-universal-time))
         (post-object (concatenate 'list
                                   (parse-post-file (getf post :filename))
                                   (get-date-struct (getf post :created-at))
                                   post)))
    (update-post post-object)
    (setf (getf post :updated-at) ts)
    post))

(defun update-post-from-file (filename)
  (let* ((ts (get-universal-time))
         (post (copy-list (car (remove-if-not
                    #'(lambda (x) (equal (getf x :filename) filename))
                    *posts*))))
         (post-object (concatenate 'list
                                   (parse-post-file filename)
                                   (get-date-struct (getf post :created-at))
                                   post)))
    (update-post post-object)
    (setf (getf post :updated-at) ts)
    post))

(defun add-props (plist name val &rest fields)
  (setf (getf plist name) val)
  (if fields
      (apply #'add-props plist fields)
      plist))

(defun xmember (struct member)
  (s-xml-rpc:get-xml-rpc-struct-member struct member))

(defun process-post-struct (post)
  (list
   :itemid (xmember post :|itemid|)
   :anum (xmember post :|anum|)
   :ditemid (xmember post :|ditemid|)
   :url (xmember post :|url|)
   ))

(defun decorate-with-context (struct filename timestamp)
  (add-props struct
             :filename filename
             :created-at timestamp))
