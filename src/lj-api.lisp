(in-package :cl-user)
(defpackage cl-journal.lj-api
  (:use :cl :s-xml-rpc)
  (:import-from :cl-journal.functions :get-date-struct)
  (:import-from :cl-journal.db :to-xmlrpc-struct :<post-file> :<post> :read-from-file :create-post-from-xmlrpc-struct :filename)
  (:export :create-new-post :update-old-post :parse-post-answer :update-post :delete-post :*livejournal-login* :*livejournal-password*))

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

(defun add-mask (req)
  (if (find "usemask" req :test #'equal)
      (concatenate 'list
                   req
                   (list "allowmask" 1))
      req))


(defun login ()
  (let* ((struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge)))
         (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.login" struct)))
    (s-xml-rpc:xml-rpc-call request
                            :url "/interface/xmlrpc"
                            :host "www.livejournal.com")))

(defun add-props (plist)
  (let* ((file-fields (list :music :mood :location :tags))
         (lj-fields (list "current_music" "current_mood" "current_location" "taglist"))
         (props (mapcar #'(lambda (field lj-field)
                            (if (getf plist field)
                                (list lj-field (getf plist field)) ()))
                        file-fields lj-fields)))
    (apply #'s-xml-rpc:xml-rpc-struct
           (apply #'concatenate 'list props))))

(defun editevent (plist)
  (let* ((params (list "event" (getf plist :body)
                       "subject" (or (getf plist :title) "")
                       "security" (get-privacy-setting plist)
                       "itemid" (getf plist :itemid)
                       "year" (getf plist :year)
                       "mon" (getf plist :mon)
                       "day" (getf plist :day)
                       "hour" (getf plist :hour)
                       "min" (getf plist :min)
                       "props" (add-props plist)))
         (struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge (add-mask params))))
         (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.editevent" struct)))
    (s-xml-rpc:xml-rpc-call request
                            :url "/interface/xmlrpc"
                            :host "www.livejournal.com")))

(defun get-privacy-setting (plist)
  (let ((privacy (or (getf plist :privacy) "public")))
    (cond
      ((string= privacy "public") "public")
      ((string= privacy "friends") "usemask")
      ((string= privacy "private") "private")
      (t "public")
      )))


(defun delete-post (plist)
  (editevent (concatenate 'list
                          (get-date-struct)
                          (list :body ""
                                :itemid (getf plist :itemid)))))

(defgeneric create-new-post (post))

(defmethod create-new-post ((post string))
  (create-new-post (read-from-file post)))

(defmethod create-new-post ((post <post-file>))
  (let* ((request (s-xml-rpc:encode-xml-rpc-call
                   "LJ.XMLRPC.postevent"
                   (to-xmlrpc-struct post #'add-challenge)))
         (response (s-xml-rpc:xml-rpc-call request
                                           :url "/interface/xmlrpc"
                                           :host "www.livejournal.com"))
         )

    (create-post-from-xmlrpc-struct response (filename post))))


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
