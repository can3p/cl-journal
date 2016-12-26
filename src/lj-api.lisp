(in-package :cl-user)
(defpackage cl-journal.lj-api
  (:use :cl :s-xml-rpc)
  (:import-from :cl-journal.functions :get-date-struct)
  (:import-from :alexandria :compose)
  (:import-from :cl-journal.settings
                :get-password)
  (:import-from
   :cl-journal.db
   :to-xmlrpc-struct
   :<post-file>
   :<post>
   :<db>
   :login
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

(defvar *livejournal-login* nil)
(defvar *livejournal-password* nil)

(defun hash (str)
  (string-downcase (format nil "~{~2,'0x~}"
                           (coerce (md5:md5sum-string str)
                                   'list))))


(defun getchallenge ()
  (s-xml-rpc:get-xml-rpc-struct-member
   (s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.getchallenge")
                           :url "/interface/xmlrpc"
                           :host "www.livejournal.com")
   :|challenge|))

(defun add-challenge (&optional (req nil))
  (let* ((challenge (getchallenge))
         (auth-response (hash (concatenate 'string
                                           challenge
                                           (hash *livejournal-password*)))))
    (concatenate 'list
                 req
                 (list "username" *livejournal-login*
                       "auth_method" "challenge"
                       "auth_challenge" challenge
                       "auth_response" auth-response))))

;; (defun login ()
;;   (let* ((struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge)))
;;          (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.login" struct)))
;;     (s-xml-rpc:xml-rpc-call request
;;                             :url "/interface/xmlrpc"
;;                             :host "www.livejournal.com")))

(defgeneric create-new-post (post))

(defmethod create-new-post ((post <post-file>))
  (let* ((request (s-xml-rpc:encode-xml-rpc-call
                   "LJ.XMLRPC.postevent"
                   (to-xmlrpc-struct post #'add-challenge)))
         (response (s-xml-rpc:xml-rpc-call request
                                           :url "/interface/xmlrpc"
                                           :host "www.livejournal.com"))
         )

    (create-post-from-xmlrpc-struct response (filename post) (journal post))))


(defgeneric delete-old-post (post))

(defmethod delete-old-post ((post <post>))
  (let* ((request (s-xml-rpc:encode-xml-rpc-call
                   "LJ.XMLRPC.editevent"
                   (to-xmlrpc-struct post #'add-challenge t)))
         (response (s-xml-rpc:xml-rpc-call request
                                           :url "/interface/xmlrpc"
                                           :host "www.livejournal.com"))
         )

    response))

(defgeneric update-old-post (post))

(defmethod update-old-post ((post <post>))
  (let* ((request (s-xml-rpc:encode-xml-rpc-call
                   "LJ.XMLRPC.editevent"
                   (to-xmlrpc-struct post #'add-challenge)))
         (response (s-xml-rpc:xml-rpc-call request
                                           :url "/interface/xmlrpc"
                                           :host "www.livejournal.com"))
         )

    response))

(defun set-credentials (db)
  (when (or (not (string= (login db) *livejournal-login*))
          (string= "" *livejournal-password*))
      (setf *livejournal-login* (login db))
      (setf *livejournal-password* (get-password (login db)))))


(defgeneric publish-post (db post))

(defmethod publish-post ((db <db>) (post-file <post-file>))
  (set-credentials db)
  (let ((post (create-new-post post-file)))
    (push post (posts db))))

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
  (update-old-post post)
  (setf (updated-at post) (get-universal-time)))
