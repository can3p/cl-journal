(in-package :rpc4cl)

;; monkey patch encode string to send properly encoded strings
(defun encode-string (string)
  (with-output-to-string (s)
    (s-xml:print-string-xml string s)))

(in-package :cl-user)
(defpackage cl-journal.lj-api
  (:use :cl :cl-arrows)
  (:import-from :cl-journal.functions :get-date-struct)
  (:import-from :alexandria :compose)
  (:import-from :cl-journal.settings
                :get-password)
  (:import-from
   :cl-journal.db
   :to-xmlrpc-struct
   :*raw-text*
   :raw-text
   :<post-file>
   :<post>
   :<db>
   :login
   :service-url
   :service-endpoint
   :read-from-file
   :create-post-from-xmlrpc-struct
   :filename
   :journal
   :updated-at
   :posts)
  (:export :publish-post
           :update-post
           :delete-post
))

(in-package :cl-journal.lj-api)

(defvar *service-login* nil)
(defvar *service-password* nil)
(defvar *service-endpoint* nil)

(defun rpc-call (method &rest method-parameters)
  (apply #'rpc4cl:rpc-call *service-endpoint*
         nil nil method method-parameters))

(defun hash (str)
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-string str)
                                   'list))))


(defun getchallenge ()
  (->
   (rpc-call "LJ.XMLRPC.getchallenge")
   (getf :challenge)))

(defun add-challenge (&optional (req nil))
  (let* ((challenge (getchallenge))
         (auth-response (hash (concatenate 'string
                                           challenge
                                           (hash *service-password*)))))
    (concatenate 'list
                 req
                 (list :username *service-login*
                       :auth_method "challenge"
                       :auth_challenge challenge
                       :auth_response auth-response))))

(defgeneric create-new-post (post))

(defmethod create-new-post ((post <post-file>))
  (let* ((response (rpc-call "LJ.XMLRPC.postevent"
                             (to-xmlrpc-struct post #'add-challenge))))
    (create-post-from-xmlrpc-struct response
                                    (filename post)
                                    (journal post))))


(defgeneric delete-old-post (post))

(defmethod delete-old-post ((post <post>))
  (rpc-call "LJ.XMLRPC.editevent"
            (to-xmlrpc-struct post #'add-challenge t)))

(defgeneric update-old-post (post))

(defmethod update-old-post ((post <post>))
  (rpc-call "LJ.XMLRPC.editevent"
            (to-xmlrpc-struct post #'add-challenge)))

(defun set-credentials (db)
  (when (or (not (string= (login db) *service-login*))
            (not (string= (service-endpoint db) *service-endpoint*))
          (string= "" *service-password*))
    (setf *service-endpoint* (service-endpoint db))
    (setf *service-login* (login db))
    (setf *service-password* (get-password (login db) (service-url db)))))

(defgeneric publish-post (db post))

(defmethod publish-post ((db <db>) (post-file <post-file>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (let ((post (create-new-post post-file)))
      (push post (posts db)))))

(defgeneric delete-post (db post))

(defmethod delete-post ((db <db>) (post <post>))
  (set-credentials db)
  (delete-old-post post)
  (setf (posts db)
        (remove-if #'(lambda (p) (string= (filename p) (filename post)))
                   (posts db))))

(defgeneric update-post (db post))

(defmethod update-post ((db <db>) (post <post>))
  (set-credentials db)
  (let ((*raw-text* (raw-text db)))
    (update-old-post post))
  (setf (updated-at post) (get-universal-time)))
