(in-package #:lj-api)

(defparameter *user* "")
(defparameter *password* "")

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
                                           (hash *password*)))))
    (concatenate 'list
                 req
                 (list "username" *user*
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


(defun login ()
  (let* ((struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge)))
         (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.login" struct)))
    (s-xml-rpc:xml-rpc-call request
                            :url "/interface/xmlrpc"
                            :host "www.livejournal.com")))

(defun postevent (event subject)
  (let* ((params (list "event" event
                       "subject" subject))
         (struct (apply #'s-xml-rpc:xml-rpc-struct (add-challenge (add-date params))))
         (request (s-xml-rpc:encode-xml-rpc-call "LJ.XMLRPC.postevent" struct)))
    (s-xml-rpc:xml-rpc-call request
                            :url "/interface/xmlrpc"
                            :host "www.livejournal.com")))


(defun create-post (plist)
  (postevent (getf plist :body)
             (or (getf plist :title) "")))
