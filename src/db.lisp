(in-package :cl-user)

(defpackage cl-journal.db
  (:use :cl :s-xml-rpc)
  (:import-from :cl-journal.functions :get-date-struct-str)
  (:import-from :cl-journal.file-api :parse-post-file)
  (:export :<post-file>
   :<post>
   :<db>
   :to-xmplrpc-struct
   :read-from-file
   :create-post-from-xmlrpc-struct
   :create-db-from-list
   :to-list
   :get-by-fname
   :filename
   :title
   :url
   :get-modified
   :get-deleted
   :updated-at
   :draft)
  )

(in-package :cl-journal.db)

(defvar *add-date-ts* nil)

(defun xmember (struct member)
  (s-xml-rpc:get-xml-rpc-struct-member struct member))

(defclass <post-file> ()
  (
   (draft :initarg :draft :initform nil :reader draft)
   (filename :initarg :filename :reader filename)
   (title :initarg :title :reader title)
   (body :initarg :body :reader body)
   (privacy :initarg :privacy :reader privacy)
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
                   :fields (alexandria:remove-from-plist parsed :title :body :privacy)
                   )))

(defmethod to-event-list ((post <post-file>) &optional (transform #'identity))
  (let ((l (list
            "event" (body post)
            "subject" (title post)
            "props" (add-props (fields post))
            )))
    (funcall transform (add-date (add-privacy-fields l (privacy post))))))

(defmethod to-xmlrpc-struct ((post <post-file>) &optional (transform #'identity))
  (let ((l (to-event-list post transform)))
    (setf (getf l "props")
          (apply #'s-xml-rpc:xml-rpc-struct (getf l "props")))
    (apply #'s-xml-rpc:xml-rpc-struct l)))

(defclass <post> ()
  (
   (updated-at :initarg :updated-at :initform (get-universal-time) :accessor updated-at)
   (created-at :initarg :created-at :initform (get-universal-time) :reader created-at)
   (filename :initarg :filename :reader filename)
   (itemid :initarg :itemid :reader itemid)
   (anum :initarg :anum :reader anum)
   (ditemid :initarg :ditemid :reader ditemid)
   (url :initarg :url :reader url)
   ))

(defgeneric to-event-list (post &optional transform))
(defgeneric to-xmlrpc-struct (post &optional transform))

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

(defun create-post-from-xmlrpc-struct (struct fname)
  (make-instance '<post>
                 :itemid (xmember struct :|itemid|)
                 :anum (xmember struct :|anum|)
                 :ditemid (xmember struct :|ditemid|)
                 :url (xmember struct :|url|)
                 :filename fname
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
   ))

(defmethod to-xmlrpc-struct ((post <post>) &optional (transform #'identity))
  (let ((*add-date-ts* (created-at post)))
    (to-xmlrpc-struct (read-from-file (filename post)) transform)))

(defclass <db> ()
  ((posts :initarg :posts :accessor posts)))

(defmethod print-object ((db <db>) stream)
  (format stream "<db ~a>~%" (posts db)))


(defgeneric get-modified (db))

(defmethod get-modified ((db <db>))
  (remove-if-not #'modified-p (posts db)))


(defgeneric get-deleted (db))

(defmethod get-deleted ((db <db>))
  (remove-if-not #'deleted-p (posts db)))

(defmethod to-list ((db <db>))
  (mapcar #'to-list (posts db)))

(defmethod get-by-fname ((db <db>) fname)
  (find-if #'(lambda (post) (get-by-fname post fname))
        (posts db)))

(defun create-db-from-list (l)
  (make-instance '<db>
                 :posts (mapcar #'create-post-from-list l)))

(defun add-props (plist)
  (let* ((file-fields (list :music :mood :location :tags))
         (lj-fields (list "current_music" "current_mood" "current_location" "taglist"))
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
                     (list "security" lj-privacy "allowmask" 1))
        (concatenate 'list
                     plist
                     (list "security" lj-privacy)))))

(defun add-date (plist &optional (ts *add-date-ts*))
  (concatenate 'list
               plist
               (get-date-struct-str ts)))
