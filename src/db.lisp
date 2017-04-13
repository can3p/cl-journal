(in-package :cl-user)

(defpackage cl-journal.db
  (:use :cl :cl-arrows)
  (:import-from :cl-journal.functions
                :prompt-read
                :get-date-struct)
  (:import-from :cl-journal.file-api :parse-post-file)
  (:import-from :alexandria :curry :compose)
  (:export :<post-file>
   :<post>
   :<db>
   :to-xmplrpc-struct
   :read-from-file
   :create-post-from-xmlrpc-struct
   :create-empty-db
   :create-db-from-list
   :login
   :to-list
   :get-by-fname
   :filename
   :title
   :journal
   :url
   :get-modified
   :get-deleted
   :updated-at
   :get-last-published-post
   :draft)
  )

(in-package :cl-journal.db)

(defvar *add-date-ts* nil)

(defclass <post-file> ()
  (
   (draft :initarg :draft :initform nil :reader draft)
   (filename :initarg :filename :reader filename)
   (title :initarg :title :reader title)
   (body :initarg :body :reader body)
   (privacy :initarg :privacy :reader privacy)
   (journal :initarg :journal :reader journal)
   (fields :initarg :fields :reader fields)
   ))

(defmethod print-object ((post-file <post-file>) stream)
  (format stream "<post-file filename:~a>~%" (filename post-file)))

(defun read-from-file (fname)
  (let ((parsed (parse-post-file fname)))
    (make-instance '<post-file>
                   :filename fname
                   :draft (getf parsed :draft)
                   :title (getf parsed :title)
                   :body (getf parsed :body)
                   :privacy (getf parsed :privacy)
                   :journal (getf parsed :journal)
                   :fields (alexandria:remove-from-plist parsed :title :body :privacy)
                   )))

(defmethod to-event-list ((post <post-file>) &optional (transform #'identity))
  (let ((l (list
            :event (body post)
            :subject (title post)
            :props (add-props (fields post))
            )))
    (-<> l
         (add-privacy-fields (privacy post))
         (add-usejournal (journal post) <>)
         (add-date)
         (funcall transform <>))))

(defmethod to-xmlrpc-struct ((post <post-file>) &optional (transform #'identity) (is-deleted nil))
  (declare (ignore is-deleted))
  (to-event-list post transform))

(defclass <post> ()
  (
   (updated-at :initarg :updated-at :initform (get-universal-time) :accessor updated-at)
   (created-at :initarg :created-at :initform (get-universal-time) :reader created-at)
   (filename :initarg :filename :reader filename)
   (journal :initarg :journal :initform nil :reader journal)
   (itemid :initarg :itemid :reader itemid)
   (anum :initarg :anum :reader anum)
   (ditemid :initarg :ditemid :reader ditemid)
   (url :initarg :url :reader url)
   ))

(defgeneric to-event-list (post &optional transform))
(defgeneric to-xmlrpc-struct (post &optional transform is-deleted))

(defmethod print-object ((post <post>) stream)
  (format stream "<post filename:~a url:~a>~%" (filename post) (url post)))

(defmethod title ((post <post>))
  (title (read-from-file (filename post))))

(defgeneric get-by-fname (post fname))

(defmethod get-by-fname ((post <post>) fname)
  (when (string= (filename post) fname) post))

(defgeneric modified-p (post))

(defmethod modified-p ((post <post>))
  (let ((fname (filename post)))
    (and
     (probe-file fname)
     (< (or (updated-at post) (created-at post))
        (file-write-date fname)))))

(defgeneric deleted-p (post))

(defmethod deleted-p ((post <post>))
  (not (probe-file (filename post))))

(defun create-post-from-xmlrpc-struct (struct fname journal)
  (make-instance '<post>
                 :itemid (getf struct :itemid)
                 :anum (getf struct :anum)
                 :ditemid (getf struct :ditemid)
                 :url (getf struct :url)
                 :filename fname
                 :journal journal
                 ))

(defun create-post-from-list (plist)
  (make-instance '<post>
                 :itemid (getf plist :itemid)
                 :anum (getf plist :anum)
                 :ditemid (getf plist :ditemid)
                 :url (getf plist :url)
                 :created-at (getf plist :created-at)
                 :updated-at (getf plist :updated-at)
                 :filename (getf plist :filename)
                 :journal (getf plist :journal)
                 ))

(defgeneric to-list (post))

(defmethod to-list ((post <post>))
  (list
   :itemid (itemid post)
   :anum (anum post)
   :ditemid (ditemid post)
   :url (url post)
   :created-at (created-at post)
   :updated-at (updated-at post)
   :filename (filename post)
   :journal (journal post)
   ))

(defun add-itemid (post req)
  (concatenate 'list
               req
               (list :itemid (itemid post))))

(defmethod to-xmlrpc-struct ((post <post>) &optional (transform #'identity) (is-deleted nil))
  (let ((*add-date-ts* (created-at post)))
    (if is-deleted
        (funcall (compose #'add-date
                          transform
                          (curry #'add-usejournal (journal post))
                          (curry #'add-itemid post))
                 '(:event "" :subject ""))
        (let ((post-file (read-from-file (filename post))))
          (if (string= (journal post-file) (journal post))
              (to-xmlrpc-struct post-file
                                (compose (curry #'add-itemid post)
                                         transform))
              (error "Post journal has been changed after post creation! Initial value was ~a and currently it's ~a. We do not support that now, please use initial value." (journal post) (journal post-file)))))))

(defclass <db> ()
  ((posts :initarg :posts :accessor posts)
   (version :initarg :version :reader version)
   (login :initarg :login :reader login)))

(defmethod print-object ((db <db>) stream)
  (format stream "<db ~a>~%" (posts db)))

(defgeneric get-modified (db))

(defmethod get-modified ((db <db>))
  (remove-if-not #'modified-p (posts db)))


(defgeneric get-deleted (db))

(defmethod get-deleted ((db <db>))
  (remove-if-not #'deleted-p (posts db)))

(defmethod to-list ((db <db>))
  `(:login ,(login db)
    :version 1
    :posts ,(mapcar #'to-list (posts db))))

(defmethod get-by-fname ((db <db>) fname)
  (find-if #'(lambda (post) (get-by-fname post fname))
        (posts db)))

(defun get-last-published-post (db)
  (car (sort (posts db) #'> :key #'created-at)))

(defun create-empty-db ()
  (let ((login (prompt-read "Please enter you livejournal login")))
    (when (string= "" login) (error "Cannot proceed without login"))
    (create-db-from-list `(:login ,login
                           :version 1
                           :posts ()))))

(defun create-db-from-list (l)
  (cond
    ((null (find :version l)) (create-db-from-list (migrate-db-v0-v1 l)))
    (t (create-db-from-list-finally l))))

(defun migrate-db-v0-v1 (l)
  (format t "You've used an outdated version of client and we need to migrate data~%")
  (let ((login (prompt-read "Please enter you livejournal login again")))
    (when (string= "" login) (error "Cannot proceed without login"))
    `(:login ,login
      :version 1
      :posts ,l)))

(defun create-db-from-list-finally (l)
  (make-instance '<db>
                 :version (getf l :version)
                 :login (getf l :login)
                 :posts (mapcar #'create-post-from-list
                                (getf l :posts))))

(defun add-props (plist)
  (let* ((file-fields (list :music :mood :location :tags))
         (lj-fields (list :current_music :current_mood :current_location :taglist))
         (props (mapcar #'(lambda (field lj-field)
                            (if (getf plist field)
                                (list lj-field (getf plist field)) ()))
                        file-fields lj-fields)))
    (apply #'concatenate 'list props)))

(defun add-privacy-fields (plist privacy)
  (let ((lj-privacy (cond
                   ((string= privacy "public") "public")
                   ((string= privacy "friends") "usemask")
                   ((string= privacy "private") "private")
                   (t "public")
                   )))
    (if (string= privacy "friends")
        (concatenate 'list
                     plist
                     (list :security lj-privacy :allowmask 1))
        (concatenate 'list
                     plist
                     (list :security lj-privacy)))))

(defun add-date (plist &optional (ts *add-date-ts*))
  (concatenate 'list
               plist
               (get-date-struct ts)))

(defun add-usejournal (journal plist)
  (if (not (null journal))
      (concatenate 'list
                   plist
                   (list :usejournal journal))
      plist))
