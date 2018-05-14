(in-package :cl-user)

(defpackage cl-journal.db
  (:use :cl :cl-arrows)
  (:import-from :cl-journal.functions
                :prompt-read
                :get-date-struct)
  (:import-from :cl-journal.file-api :parse-post-file)
  (:import-from :alexandria :curry :compose)
  (:import-from :cl-strings :join :split)
  (:import-from :cl-slug :slugify)
  (:import-from :local-time
   :timestamp>
   :parse-timestring)
  (:export :<post-file>
   :<post>
   :*raw-text*
   :raw-text
   :<db>
   :to-xmplrpc-struct
   :read-from-file
   :create-post-from-xmlrpc-struct
   :create-empty-db
   :create-db-from-list
   :login
   :events
   :service-endpoint
   :service-url
   :older-than-p
   :to-list
   :get-by-fname
   :last-post-ts
   :filename
   :title
   :journal
   :url
   :posts
   :get-modified
   :get-deleted
   :updated-at
   :ignored-at
   :get-last-published-post
   :refill-store
   :merge-events
   :fetch-store
   :to-hash-table
   :server-changed-at
   :itemid
   :draft)
  )

(in-package :cl-journal.db)

(defvar *add-date-ts* nil)
(defvar *raw-text* t)
(defvar *default-service* :livejournal)

(defun resolve-service-name (service)
  (cond
    ((string= service "livejournal") :livejournal)
    ((string= service "dreamwidth") :dreamwidth)
    (t :manual)
    ))

(defun resolve-service-endpoint (service service-name)
  (case service
    (:livejournal "http://www.livejournal.com/interface/xmlrpc")
    (:dreamwidth  "https://www.dreamwidth.org/interface/xmlrpc")
    (otherwise service-name)))

(defun service-url (db)
  (case (service db)
    (:livejournal "www.livejournal.com")
    (:dreamwidth  "www.dreamwidth.org")
    (otherwise (service-endpoint db))))

(defclass <post-file> ()
  (
   (draft :initarg :draft :initform nil :reader draft)
   (filename :initarg :filename :reader filename)
   (title :initarg :title :reader title)
   (body :initarg :body :reader body)
   (body-raw :initarg :body-raw :reader body-raw)
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
                   :body-raw (getf parsed :body-raw)
                   :privacy (getf parsed :privacy)
                   :journal (getf parsed :journal)
                   :fields (alexandria:remove-from-plist parsed :title :body :privacy)
                   )))

(defmethod to-event-list ((post <post-file>) &optional (transform #'identity))
  (let ((l (list
            :event (if *raw-text* (body-raw post) (body post))
            :subject (title post)
            )))
    (-<> l
         (add-props (fields post))
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
   (ignored-at :initarg :ignored-at :initform nil :accessor ignored-at)
   (server-changed-at :initarg :server-changed-at :initform nil :accessor server-changed-at)
   (filename :initarg :filename :reader filename)
   (journal :initarg :journal :initform nil :reader journal)
   (itemid :initarg :itemid :reader itemid)
   (anum :initarg :anum :reader anum)
   (ditemid :initarg :ditemid :reader ditemid)
   (url :initarg :url :reader url)
   ))

(defgeneric to-event-list (post &optional transform))
(defgeneric to-xmlrpc-struct (post &optional transform is-deleted))
(defgeneric to-hash-table (source &key))

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
     (< (or (ignored-at post) (updated-at post) (created-at post))
        (file-write-date fname)))))

(defgeneric deleted-p (post))

(defmethod deleted-p ((post <post>))
  (not (probe-file (filename post))))

(defgeneric older-than-p (post ts))

(defmethod older-than-p ((post <post>) ts)
  (labels ((parse (ts)
             (let
                 ((local-time::*default-timezone* local-time::+utc-zone+))
               (parse-timestring ts :date-time-separator #\Space))))
    (or (not (server-changed-at post))
        (timestamp>
         (parse ts)
         (parse (server-changed-at post))))))

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
                 :ignored-at (getf plist :ignored-at)
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
   :ignored-at (ignored-at post)
   :server-changed-at (server-changed-at post)
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
   (login :initarg :login :reader login)
   (raw-text :initarg :raw-text :reader raw-text)
   (service :initarg :service :reader service)
   (service-endpoint :initarg :service-endpoint :reader service-endpoint)
   (fetch-store :reader fetch-store)
   ))


;; in future we will populate store once object is
;; created and we don't want to do this everytime
;; we create a database. So, we do this nice
;; hack to get lazy reader
(defmethod slot-unbound (class (db <db>) (slot-name (eql 'fetch-store)))
  (setf (slot-value db 'fetch-store)
        (make-instance '<store>)))

(defgeneric get-modified (db))

(defmethod get-modified ((db <db>))
  (remove-if-not #'modified-p (posts db)))


(defgeneric get-deleted (db))

(defmethod get-deleted ((db <db>))
  (remove-if-not #'deleted-p (posts db)))

(defmethod to-list ((db <db>))
  `(:login ,(login db)
    :version 2
    :service ,(service db)
    :raw-text ,(raw-text db)
    :service-endpoint ,(service-endpoint db)
    :posts ,(mapcar #'to-list (posts db))))

(defmethod to-hash-table ((db <db>) &key (key-sub #'itemid))
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (post (posts db) ht)
        (setf (gethash (funcall key-sub post) ht) post))))

(defmethod get-by-fname ((db <db>) fname)
  (find-if #'(lambda (post) (get-by-fname post fname))
        (posts db)))

(defun generate-unique-filename (db datetime base)
  "Generate a filename that does not yet exist in the database
   based on datetime (yyyy-mm-dd hh:mm:ss) and a base (any string).

   Function will generate a a base name of the form <date>-<slug>.md
   and in case such a filename already exist, will keep adding increasing
   postfix numbers till it finds a vacant spot"
  (labels ((gen-name (date slug counter)
             (if (equal counter 1)
                 (format nil "~a-~a.md" date slug)
                 (format nil "~a-~a-~a.md" date slug counter))))

    (let ((date (car (split datetime)))
          (slug (slugify base))
          (ht (to-hash-table db :key-sub #'filename)))

      (loop with cnt = 0
            for name = (gen-name date slug (incf cnt))
            while (gethash name ht)
            finally (return name)))))

(defun get-last-published-post (db)
  (car (sort (posts db) #'> :key #'created-at)))

(defun create-empty-db (service-name)
  (let ((login (prompt-read "Please enter you login"))
        (raw-text (not (yes-or-no-p "Transform markdown into html?")))
        (service (resolve-service-name service-name)))
    (when (string= "" login) (error "Cannot proceed without login"))
    (create-db-from-list `(:login ,login
                           :version 2
                           :service ,service
                           :raw-text ,raw-text
                           :service-endpoint ,(resolve-service-endpoint
                                              service service-name)
                           :posts ()))))

(defun create-db-from-list (l)
  (cond
    ((null (find :version l)) (create-db-from-list (migrate-db-v0-v1 l)))
    ((null (find :service l)) (create-db-from-list (migrate-db-v1-v2 l)))
    (t (create-db-from-list-finally l))))

(defun migrate-db-v0-v1 (l)
  (format t "You've used an outdated version of client and we need to migrate data~%")
  (let ((login (prompt-read "Please enter you login again")))
    (when (string= "" login) (error "Cannot proceed without login"))
    `(:login ,login
      :version 1
      :posts ,l)))

(defun migrate-db-v1-v2 (l)
  (format t "You've used an outdated version of client and we need to migrate data~%")
  (concatenate 'list l
               (list
                :service *default-service*
                :service-endpoint (resolve-service-endpoint
                                   *default-service* *default-service*))))

(defun create-db-from-list-finally (l)
  (make-instance '<db>
                 :version (getf l :version)
                 :login (getf l :login)
                 :service (getf l :service)
                 :raw-text (getf l :raw-text)
                 :service-endpoint (getf l :service-endpoint)
                 :posts (mapcar #'create-post-from-list
                                (getf l :posts))))

(defun add-props (plist props-plist)
  (let* ((file-fields (list :music :mood :location :tags))
         (lj-fields (list :current_music :current_mood :current_location :taglist))
         (props (mapcar #'(lambda (field lj-field)
                            (if (getf props-plist field)
                                (list lj-field (getf props-plist field)) ()))
                        file-fields lj-fields))
         (props-list (apply #'concatenate 'list props)))
    (if (not (null props-list))
        (concatenate 'list
                     plist
                     (list :props props-list))
        plist)))

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

(defclass <store> ()
  (
   (events :initform nil :accessor events)
   (last-post-ts :initform nil :accessor last-post-ts)
   ))

(defmethod to-list ((store <store>))
  `(:events ,(events store)
    :last-post-ts ,(last-post-ts store)))

(defmethod to-hash-table ((store <store>) &key)
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (item (events store) ht)
      (let ((itemid (-<> item
                         (getf <> :event)
                         (getf <> :itemid)))
            (ts (getf item :sync-ts)))
        (setf (gethash itemid ht) ts)))))

(defun merge-events (store new-events last-item-ts)
  (setf (events store)
        (concatenate 'list
                     (events store)
                     new-events))
  (setf (last-post-ts store) last-item-ts))

(defun refill-store (store l)
  (setf (events store) (getf l :events))
  (setf (last-post-ts store) (getf l :last-post-ts)))
