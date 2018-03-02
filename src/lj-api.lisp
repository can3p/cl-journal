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
  (:import-from :local-time
   :timestamp>
   :parse-timestring)
  (:import-from
   :cl-journal.db
   :to-xmlrpc-struct
   :*raw-text*
   :raw-text
   :<post-file>
   :<post>
   :<db>
   :<store>
   :events
   :fetch-store
   :login
   :service-url
   :service-endpoint
   :read-from-file
   :last-post-ts
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

;; take last-post-ts from the store
;; call sync posts
;; filter out comments and posts that are already in db
;; and have sync date later than last-post-ts
;; repeat the process till nothing is left or we have a
;; decent amount of items
(defconstant +number-items-to-fetch+ 100)

(defun syncitems-post-p (item)
  (equal #\L (-<> item
                  (getf <> :item)
                  (char <> 0))))

(defun syncitems-same-post-p (item1 item2)
  (string= (getf item1 :item)
           (getf item2 :item)))

(defun acc (l &rest args)
  "Access member in a nested plist.

   Usage (acc l :one :two :three)"
  (cond
    ((null args) l)
    ((not (listp l)) nil)
    (t (apply #'acc (getf l (car args)) (cdr args)))))

(defun partial (func &rest args)
  "Partially apply function. Another function
   is returned that will call passed function
   with all arguments from first call + arguments
   from the second one"
  (lambda (&rest args2)
    (apply func (concatenate 'list args args2))))

(defun syncitems-newer-post-in-store-p (store item)
  (let ((itemid (-<> item
                     (getf <> :item)
                     (subseq <> 2)
                     (parse-integer))))
    (labels ((item-p (event)
               (equal itemid (acc event :event :itemid)))
             (parse (ts)
               (let
                   ((local-time::*default-timezone* local-time::+utc-zone+))
                 (parse-timestring ts :date-time-separator #\Space))))
      (let ((event (find-if #'item-p (events store) :from-end t)))
        (and event
            (timestamp>
             (parse (acc event :sync-ts))
             (parse (acc item :time)))
            )))))

(defun syncitems-item-data (l)
  "Extract timestamp and itemid from the sync item record"
  (values (parse-integer (subseq (getf l :item) 2))
          (parse-timestring (getf l :time) :date-time-separator #\Space)))


(defun get-unfetched-item-ids (store
                               &key (num-to-fetch +number-items-to-fetch+))
  "Given current state of store fetch next num-to-fetch
   items that need to be synced. Returns this number
   or lest if that's all that is available"
  (labels
      ((sync-more (l ts)
         (cond
           ((> (length l) num-to-fetch)
            (subseq l 0 num-to-fetch))
           ((= (length l) num-to-fetch) l)
           (t
            (let* ((orig-list (-<> ts
                                   (lj-syncitems)
                                   (getf <> :syncitems)))
                   (orig-list-len (length orig-list))
                   ;; we need to fetch last from the original
                   ;; list since items at the end might be removed
                   (new-ts (-<> orig-list
                                (reverse)
                                (car)
                                (getf <> :time)))
                   (new-l (-<> orig-list
                               (remove-if (partial
                                           #'syncitems-newer-post-in-store-p
                                           store) <>)
                               (remove-if-not #'syncitems-post-p <>)
                               (concatenate 'list l <>)
                               (remove-duplicates <>
                                                  :test
                                                  #'syncitems-same-post-p))))
              (if (= 0 orig-list-len) l
                  (-<> new-l
                       (concatenate 'list l <>)
                       (sync-more <> new-ts))))))))
    (sync-more nil (last-post-ts store))))

