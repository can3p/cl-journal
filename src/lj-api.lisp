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
   :<store>
   :fetch-store
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

;; source of wisdom - https://github.com/apparentlymart/livejournal/blob/master/cgi-bin/ljprotocol.pl
(defun lj-getevents (itemids)
  (let* ((c (-<> (list
                  ;; this gives us lj-embeds with video id, that we can later
                  ;; convert to something meaningful
                  :get_video_ids 1
                  :itemids (format nil "~{~s~^,~}" itemids)
                  :lineendings "unix"
                  :selecttype "multiple")
                 (add-challenge))))
    (rpc-call "LJ.XMLRPC.getevents" c)))

(defun lj-syncitems (&optional (lastsync nil))
  (let* ((c (-<> (list :ver 1)
                 (add-challenge)
                 (maybe-add-last-sync <> lastsync))))
    (rpc-call "LJ.XMLRPC.syncitems" c)))

(defun maybe-add-last-sync (l &optional (lastsync nil))
  (if lastsync
      (concatenate 'list l
                   (list :lastsync lastsync))
      l))

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

;; (def post-structure :itemid "number"
;;   :subject "maybe-encoded-string"
;;   :event "maybe-encoded-string"
;;   :allowmask 1
;;   :security "usemask"
;;   :anum "number"

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

(defgeneric fetch-posts (db))

(defmethod fetch-posts ((db <db>))
  (set-credentials db)
  (fetch-posts (fetch-store db)))

;; sync algorithm:
;; 1. Call syncitems with timestamp of last synced item or nothing
;; 2. Goto 5 if no entries were returned
;; 3. Filter out items that have not yet been downloaded, or their
;;    server timestamp is greater than timestamp of last sync
;; 4. Goto 1 if we have less than ~90 entries to download
;; 5. Finish if no entries were collected
;; 6. Download collected items via getevents
;; 7. Add last sync time to every downloaded entry.
;; 8. Append downloaded entries to saved ones and save timestamp
;;    of the last one from syncitems call as last sync timestamp
;; 9. Goto 1

;; @TODO: actually implement the code
;; (defmethod fetch-posts ((store <store>))
;;   (let* ((new-itemids (get-unfetched-item-ids store)))
;;     (when (> (length new-itemids) 0)
;;       (destructruing-bind (new-events sync-ts)
;;                           (fetch-events (last-fetched-ts store))
;;                           (merge-events store new-events sync-ts))
;;       (fetch-posts store))))
