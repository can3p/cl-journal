(in-package #:lj-api)

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

(defun add-date (&optional (req nil))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (concatenate 'list
                 req
                 (list "year" year
                       "mon" month
                       "day" date
                       "hour" hour
                       "min" minute))))

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

(defun postevent (plist)
  (let* ((params (list "event" (getf plist :body)
                       "subject" (or (getf plist :title) "")
                       "security" (get-privacy-setting plist)
                       "props" (add-props plist)))
         (struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge (add-mask (add-date params)))))
         (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.postevent" struct)))
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


(defun create-post (plist)
  (postevent plist))
